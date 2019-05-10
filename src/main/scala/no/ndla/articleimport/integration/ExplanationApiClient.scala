/*
 * Part of NDLA article-import.
 * Copyright (C) 2019 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.integration

import com.typesafe.scalalogging.LazyLogging
import no.ndla.network.NdlaClient
import scalaj.http.Http
import no.ndla.articleimport.ArticleImportProperties.Domain
import no.ndla.articleimport.caching.{Memoize, NoparamMemoize}
import org.json4s.{DefaultFormats, Formats}
import org.json4s.ext.EnumNameSerializer
import org.json4s.native.Serialization.write
import cats.implicits._
import no.ndla.articleimport.model.api.{ImportException, NotFoundException}
import no.ndla.articleimport.model.domain.Concept

import scala.util.{Failure, Success, Try}

trait ExplanationApiClient {
  this: NdlaClient =>
  val explanationApiClient: ExplanationApiClient

  class ExplanationApiClient extends LazyLogging {
    private val apiUrl = s"$Domain/concepts/api/v1"
    private val ExplanationEndPoint = s"$apiUrl/Concept"
    private val StatusEndpoint = s"$apiUrl/Status"
    private val ExplanationSearchEndPoint = s"$ExplanationEndPoint/Search"
    private val MetaDataEndpoint = s"$Domain/concepts/api/v1/MetaData"
    private val MetaDataSearchEndpoint = s"$MetaDataEndpoint/Search"
    private implicit val format: Formats = DefaultFormats + new EnumNameSerializer(ExplanationStatus)

    def createOrUpdateConcept(nids: List[String],
                              concept: Concept,
                              subject: String): Try[List[CreateConceptResponse]] = {
      val existing = concept.title.toList.traverse(c => searchWithTitle(c.title)).getOrElse(List.empty)
      val groupIds = existing.flatten.filter(_.groupId.isDefined).flatMap(_.groupId)

      if (groupIds.size > 1) {
        logger.warn(
          s"Concept with title '${concept.title.headOption.map(_.title).getOrElse("-")}' was found with multiple groupIds in explanation api.")
      }

      concept.supportedLanguages
        .foldLeft(List.empty[Try[CreateConceptResponse]])((completed, lang) => {

          // Find groupId for latest successful creation or use latest from existing
          val groupIdToUse = completed.reverse
            .find(_.isSuccess)
            .flatMap(_.toOption.flatMap(_.data.groupId))
            .orElse(groupIds.lastOption)

          // Build concept if required fields are found
          val maybeNewConcept = for {
            title <- concept.title.find(_.language == lang).map(_.title)
            content <- concept.content.find(_.language == lang).map(_.content)
            newConcept = NewImportConcept(title, content, lang, subject, groupIdToUse)
          } yield newConcept

          // Use built concept to create/update concept in explanations api
          val response = maybeNewConcept match {
            case None =>
              Failure(ImportException(
                nids.headOption.getOrElse("Missing nid"),
                s"Something went wrong when creating concept '${concept.title.headOption.map(_.title).getOrElse("-")}'"))
            case Some(c) =>
              existing.flatten.find(_.title.toLowerCase == c.title.toLowerCase) match {
                case Some(e) => updateExistingConcept(e.id, c)
                case None    => createNewConcept(c)
              }
          }
          completed :+ response
        })
        .sequence
    }

    private def searchWithTitle(title: String): Try[Option[ConceptSearchResult]] =
      getPagedResults[ConceptSearchResult](ExplanationSearchEndPoint, "title" -> title)
        .map(results => results.find(_.title.toLowerCase == title.toLowerCase))

    private def createNewConcept(newConcept: NewImportConcept) = {
      for {
        statusMap <- getStatusMap()
        languageId <- getLanguageId(newConcept.language)
        subjectId <- getSubjectId(newConcept.subjectName)
        statusId = statusMap.getOrElse(ExplanationStatus.Published, 1)

        conceptBody = CreateConceptBody(
          title = newConcept.title,
          content = newConcept.content,
          sourceAuthor = "Import",
          metaIds = List(languageId, subjectId),
          statusId = statusId,
          groupId = newConcept.groupId
        )

        response <- postWithData[CreateConceptResponse, CreateConceptBody](ExplanationEndPoint, conceptBody)
      } yield response
    }

    private def updateExistingConcept(existingId: Int, toUpdate: NewImportConcept) = {
      for {
        statusMap <- getStatusMap()
        languageId <- getLanguageId(toUpdate.language)
        subjectId <- getSubjectId(toUpdate.subjectName)
        statusId = statusMap.getOrElse(ExplanationStatus.Published, 1)

        conceptBody = CreateConceptBody(
          title = toUpdate.title,
          content = toUpdate.content,
          sourceAuthor = "Import",
          metaIds = List(languageId, subjectId),
          statusId = statusId,
          groupId = toUpdate.groupId,
          id = Some(existingId)
        )

        response <- putWithData[CreateConceptResponse, CreateConceptBody](ExplanationEndPoint, conceptBody)
      } yield response
    }

    private def getLanguageId(language: String): Try[Int] =
      getLanguageMap().flatMap(lmap =>
        lmap.get(language) match {
          case Some(languageId) => Success(languageId)
          case None             => Failure(NotFoundException(s"Could not find language with abbreviation '$language'"))
      })

    private def getSubjectId(subject: String): Try[Int] =
      getSubjectMap().flatMap(smap =>
        smap.get(subject) match {
          case Some(subjectId) => Success(subjectId)
          case None            => Failure(NotFoundException(s"Could not find subject with name '$language'"))
      })

    private lazy val getSubjectMap = NoparamMemoize(_getSubjectMap _)
    private def _getSubjectMap =
      getPagedResults[MetaDataResult](MetaDataSearchEndpoint, "Category" -> "Fag")
        .map(_.map(res => res.name -> res.id).toMap)

    private lazy val getLanguageMap = NoparamMemoize(_getLanguageMap _)
    private def _getLanguageMap =
      getPagedResults[MetaDataResult](MetaDataSearchEndpoint, "Category" -> "Språk")
        .map(_.map(res => res.abbreviation -> res.id).toMap)

    private lazy val getStatusMap = NoparamMemoize(_getStatusMap _)
    private def _getStatusMap: Try[Map[ExplanationStatus.Value, Int]] =
      getPagedResults[StatusResult](StatusEndpoint).map(_.flatMap(status =>
        ExplanationStatus.valueOf(status.name).map(statusKey => statusKey -> status.id)).toMap)

    private def getPagedResults[R](endpoint: String, params: (String, String)*): Try[List[R]] =
      get[ExplanationResponse[R]](endpoint, params :+ "PageSize" -> "100": _*).flatMap(initial => {
        (2 to initial.data.numberOfPages).toList
          .traverse(pageNum => {
            get[ExplanationResponse[R]](endpoint, params :+ "PageSize" -> "100" :+ "Page" -> s"$pageNum": _*)
          })
          .map(fet => (initial +: fet).flatMap(_.data.results))
      })

    private def postWithData[A, B <: AnyRef](endpointUrl: String, data: B, params: (String, String)*)(
        implicit mf: Manifest[A],
        format: org.json4s.Formats): Try[A] = {
      ndlaClient.fetchWithForwardedAuth[A](
        Http(endpointUrl)
          .postData(write(data))
          .method("POST")
          .params(params.toMap)
          .header("content-type", "application/json")
      )
    }

    private def putWithData[A, B <: AnyRef](endpointUrl: String, data: B, params: (String, String)*)(
        implicit mf: Manifest[A],
        format: org.json4s.Formats): Try[A] = {
      ndlaClient.fetchWithForwardedAuth[A](
        Http(endpointUrl)
          .put(write(data))
          .method("PUT")
          .params(params.toMap)
          .header("content-type", "application/json")
      )
    }

    private def get[A](url: String, params: (String, String)*)(implicit mf: Manifest[A]): Try[A] =
      ndlaClient.fetchWithForwardedAuth[A](Http(url).timeout(20000, 20000).params(params)) match {
        case Success(x)  => Success(x)
        case Failure(ex) => Failure(ex)
      }

    object ExplanationStatus extends Enumeration {
      val Published: Value = Value("Publisert")
      val Utkast: Value = Value("Utkast")
      val Arkivert: Value = Value("Arkivert")
      val Utgatt: Value = Value("Utgått")

      def valueOf(key: String): Option[ExplanationStatus.Value] = {
        values.find(_.toString == key)
      }
    }

    case class ExplanationResponse[T](data: ExplanationData[T])
    case class ExplanationData[T](results: List[T], page: Int, pageSize: Int, numberOfPages: Int, totalItems: Int)

    case class StatusResult(id: Int, name: String)
    case class MetaDataResult(id: Int, name: String, abbreviation: String)
    case class ConceptSearchResult(id: Int, externalId: String, title: String, groupId: Option[String])

    case class NewImportConcept(title: String,
                                content: String,
                                language: String,
                                subjectName: String,
                                groupId: Option[String])

    case class CreateConceptResponse(data: CreateConceptResponseData)
    case class CreateConceptResponseData(id: Int, groupId: Option[String])

    case class CreateConceptBody(
        title: String,
        content: String,
        sourceAuthor: String,
        metaIds: List[Int],
        statusId: Int,
        groupId: Option[String],
        externalId: Int = -2,
        media: List[Int] = List.empty,
        id: Option[Int] = None
    )

  }

}
