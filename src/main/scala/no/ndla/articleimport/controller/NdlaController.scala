/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.controller

import javax.servlet.http.HttpServletRequest

import com.typesafe.scalalogging.LazyLogging
import no.ndla.articleimport.ArticleImportProperties.{CorrelationIdHeader, CorrelationIdKey}
import no.ndla.articleimport.model.api.{
  AccessDeniedException,
  Error,
  ImportException,
  ImportExceptions,
  NotFoundException,
  OptimisticLockException,
  ValidationError
}
import no.ndla.articleimport.model.domain.{ImportError, emptySomeToNone}
import no.ndla.articleimport.service.ConverterService
import no.ndla.network.model.HttpRequestException
import no.ndla.network.{ApplicationUrl, AuthUser, CorrelationID}
import no.ndla.validation.{ValidationException, ValidationMessage}
import org.apache.logging.log4j.ThreadContext
import org.json4s.native.Serialization.read
import org.json4s.{DefaultFormats, Formats}
import org.scalatra.json.NativeJsonSupport
import org.scalatra.{BadRequest, InternalServerError, NotFound, ScalatraServlet, _}

import scala.util.{Failure, Success, Try}

trait NdlaController {
  this: ConverterService =>

  abstract class NdlaController extends ScalatraServlet with NativeJsonSupport with LazyLogging {
    protected implicit override val jsonFormats: Formats = DefaultFormats

    before() {
      contentType = formats("json")
      CorrelationID.set(Option(request.getHeader(CorrelationIdHeader)))
      ThreadContext.put(CorrelationIdKey, CorrelationID.get.getOrElse(""))
      ApplicationUrl.set(request)
      AuthUser.set(request)
      logger.info("{} {}{}",
                  request.getMethod,
                  request.getRequestURI,
                  Option(request.getQueryString).map(s => s"?$s").getOrElse(""))
    }

    after() {
      CorrelationID.clear()
      ThreadContext.remove(CorrelationIdKey)
      AuthUser.clear()
      ApplicationUrl.clear
    }

    error {
      case a: AccessDeniedException =>
        Forbidden(body = Error(Error.ACCESS_DENIED, a.getMessage))
      case v: ValidationException =>
        BadRequest(body = ValidationError(messages = v.errors))
      case n: NotFoundException =>
        NotFound(body = Error(Error.NOT_FOUND, n.getMessage))
      case o: OptimisticLockException =>
        Conflict(body = Error(Error.RESOURCE_OUTDATED, o.getMessage))
      case i: ImportExceptions =>
        UnprocessableEntity(body = converterService.generateImportErrorMessage(i))
      case im: ImportException =>
        UnprocessableEntity(body = converterService.generateImportError(im))
      case h: HttpRequestException =>
        h.httpResponse match {
          case Some(resp) if resp.is4xx => BadRequest(body = resp.body)
          case _ =>
            logger.error(s"Problem with remote service: ${h.getMessage}")
            BadGateway(body = Error.GenericError)
        }
      case t: Throwable =>
        logger.error(Error.GenericError.toString, t)
        InternalServerError(body = Error.GenericError)
    }

    def long(paramName: String)(implicit request: HttpServletRequest): Long = {
      val paramValue = params(paramName)
      paramValue.forall(_.isDigit) match {
        case true => paramValue.toLong
        case false =>
          throw new ValidationException(
            errors = Seq(ValidationMessage(paramName, s"Invalid value for $paramName. Only digits are allowed.")))
      }
    }

    def paramOrNone(paramName: String)(implicit request: HttpServletRequest): Option[String] = {
      params.get(paramName).map(_.trim).filterNot(_.isEmpty())
    }

    def paramOrDefault(paramName: String, default: String)(implicit request: HttpServletRequest): String = {
      paramOrNone(paramName).getOrElse(default)
    }

    def intOrNone(paramName: String)(implicit request: HttpServletRequest): Option[Int] =
      paramOrNone(paramName).flatMap(p => Try(p.toInt).toOption)

    def intOrDefault(paramName: String, default: Int): Int =
      intOrNone(paramName).getOrElse(default)

    def paramAsListOfString(paramName: String)(implicit request: HttpServletRequest): List[String] = {
      emptySomeToNone(params.get(paramName)) match {
        case None        => List.empty
        case Some(param) => param.split(",").toList.map(_.trim)
      }
    }

    def paramAsListOfLong(paramName: String)(implicit request: HttpServletRequest): List[Long] = {
      val strings = paramAsListOfString(paramName)
      strings.headOption match {
        case None => List.empty
        case Some(_) =>
          if (!strings.forall(entry => entry.forall(_.isDigit))) {
            throw new ValidationException(
              errors =
                Seq(ValidationMessage(paramName, s"Invalid value for $paramName. Only (list of) digits are allowed.")))
          }
          strings.map(_.toLong)
      }
    }

    def booleanOrNone(paramName: String)(implicit request: HttpServletRequest): Option[Boolean] =
      paramOrNone(paramName).flatMap(p => Try(p.toBoolean).toOption)

    def booleanOrDefault(paramName: String, default: Boolean)(implicit request: HttpServletRequest): Boolean =
      booleanOrNone(paramName).getOrElse(default)

    def extract[T](json: String)(implicit mf: scala.reflect.Manifest[T]): T = {
      Try {
        read[T](json)
      } match {
        case Failure(e) => {
          logger.error(e.getMessage, e)
          throw new ValidationException(errors = Seq(ValidationMessage("body", e.getMessage)))
        }
        case Success(data) => data
      }
    }

  }

}
