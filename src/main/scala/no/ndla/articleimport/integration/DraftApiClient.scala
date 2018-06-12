/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.integration

import no.ndla.articleimport.ArticleImportProperties
import no.ndla.articleimport.model.api._
import no.ndla.articleimport.model.domain.{Article, ArticleIds, Concept, Language}
import no.ndla.network.NdlaClient
import no.ndla.articleimport.model.api
import no.ndla.articleimport.service.ConverterService
import org.joda.time.DateTime

import scala.util.{Failure, Success, Try}
import scalaj.http.Http
import org.json4s.native.Serialization.write

trait DraftApiClient {
  this: NdlaClient with ConverterService =>
  val draftApiClient: DraftApiClient

  class DraftApiClient {
    implicit val formats = org.json4s.DefaultFormats

    private val DraftApiInternEndpoint =
      s"http://${ArticleImportProperties.DraftHost}/intern"
    private val DraftApiPublicEndpoint =
      s"http://${ArticleImportProperties.DraftHost}/draft-api/v1/drafts"
    private val DraftApiConceptPublicEndpoint =
      s"http://${ArticleImportProperties.DraftHost}/draft-api/v1/concepts"
    private val DraftHealthEndpoint =
      s"http://${ArticleImportProperties.DraftHost}/health"

    def getContentByExternalId(externalId: String): Option[api.ApiContent] = {
      getArticleIdFromExternalId(externalId)
        .flatMap(getArticleFromId)
        .orElse(getConceptIdFromExternalId(externalId).flatMap(getConceptFromId))
    }

    def getArticleFromId(id: Long): Option[api.Article] = {
      get[api.Article](s"$DraftApiPublicEndpoint/$id").toOption
    }

    def getArticleIdFromExternalId(externalId: String): Option[Long] = {
      get[ContentId](s"$DraftApiPublicEndpoint/external_id/$externalId")
        .map(_.id)
        .toOption
    }

    def newArticle(article: NewArticle,
                   mainNodeId: String,
                   subjectIds: Set[String],
                   createdDate: String,
                   updatedDate: String): Try[api.Article] = {
      postWithData[api.Article, NewArticle](
        s"$DraftApiPublicEndpoint/",
        article,
        "externalId" -> mainNodeId,
        "externalSubjectIds" -> subjectIds.mkString(","),
        "oldNdlaCreatedDate" -> createdDate,
        "oldNdlaUpdatedDate" -> updatedDate
      )
    }

    def newArticle(article: Article, mainNodeId: String, subjectIds: Set[String]): Try[api.Article] = {
      val newArt = converterService.toApiNewArticle(article, article.supportedLanguages.head)
      val updateArticles = article.supportedLanguages
        .drop(1)
        .zipWithIndex
        .map {
          case (lang, idx) =>
            converterService.toApiUpdateArticle(article, lang, idx + 1)
        }

      val createdTime = new DateTime(article.created).toString
      val updatedTime = new DateTime(article.updated).toString

      newArticle(newArt, mainNodeId, subjectIds, createdTime, updatedTime) match {
        case Success(a) =>
          val (failed, _) = updateArticles
            .map(u => updateArticle(u, a.id, mainNodeId, subjectIds, createdTime, updatedTime))
            .partition(_.isFailure)
          if (failed.nonEmpty) {
            val failedMsgs = failed.map(_.failed.get.getMessage).mkString(", ")
            Failure(ImportException(s"${a.id}", s"Failed to update one or more article: $failedMsgs"))
          } else {
            Success(a)
          }
        case Failure(ex) => Failure(ex)
      }
    }

    def newEmptyArticle(mainNodeId: String, subjectIds: Set[String]): Try[ContentId] =
      post[ContentId](s"$DraftApiInternEndpoint/empty_article/",
                      "externalId" -> mainNodeId,
                      "externalSubjectIds" -> subjectIds.mkString(","))

    private def updateArticle(article: api.UpdateArticle,
                              id: Long,
                              mainNodeId: String,
                              externalSubjectIds: Set[String],
                              created: String,
                              updated: String): Try[api.Article] = {
      patch[api.Article, api.UpdateArticle](
        s"$DraftApiPublicEndpoint/$id",
        article,
        "externalId" -> mainNodeId,
        "externalSubjectIds" -> externalSubjectIds.mkString(","),
        "oldNdlaCreatedDate" -> created,
        "oldNdlaUpdatedDate" -> updated
      )
    }

    private def updateArticle(article: api.UpdateArticle,
                              mainNodeId: String,
                              subjectIds: Set[String],
                              created: String,
                              updated: String): Try[api.Article] = {
      getArticleIdFromExternalId(mainNodeId) match {
        case Some(id) => updateArticle(article, id, mainNodeId, subjectIds, created, updated)
        case None =>
          Failure(NotFoundException(s"No article with external id $mainNodeId found"))
      }
    }

    def updateArticle(article: Article, mainNodeId: String, externalSubjectIds: Set[String]): Try[api.Article] = {
      val startRevision =
        getContentByExternalId(mainNodeId).flatMap(_.revision).getOrElse(1)
      val updateArticles = article.supportedLanguages.zipWithIndex
        .map {
          case (lang, idx) =>
            converterService.toApiUpdateArticle(article, lang, startRevision + idx)
        }

      val createdTime = new DateTime(article.created).toString
      val updatedTime = new DateTime(article.updated).toString

      val (failed, updatedArticle) = updateArticles
        .map(u => updateArticle(u, mainNodeId, externalSubjectIds, createdTime, updatedTime))
        .partition(_.isFailure)
      if (failed.nonEmpty) {
        val failedMsg = failed.map(_.failed.get.getMessage).mkString(", ")
        Failure(ImportException(s"${article.id}", s"Failed to update one or more article: $failedMsg"))
      } else {
        updatedArticle.head
      }
    }

    def publishArticle(id: Long): Try[ArticleStatus] = {
      for {
        _ <- put[ArticleStatus](s"$DraftApiPublicEndpoint/$id/publish/?import_publish=true")
        status <- post[ArticleStatus](s"$DraftApiInternEndpoint/article/$id/publish/?import_publish=true")
      } yield status
    }

    def deleteArticle(id: Long): Try[ContentId] =
      delete[ContentId](s"$DraftApiInternEndpoint/article/$id/")

    private def getConceptFromId(id: Long): Option[api.Concept] = {
      get[api.Concept](s"$DraftApiConceptPublicEndpoint/$id").toOption
    }

    private def newConcept(concept: NewConcept, mainNodeId: String): Try[api.Concept] = {
      postWithData[api.Concept, NewConcept](s"$DraftApiConceptPublicEndpoint/", concept, "externalId" -> mainNodeId)
    }

    def newConcept(concept: Concept, mainNodeId: String): Try[api.Concept] = {
      val newCon: api.NewConcept = converterService.toNewApiConcept(concept,
                                                                    concept.supportedLanguages.headOption
                                                                      .getOrElse(Language.UnknownLanguage))
      val updateCons = concept.supportedLanguages
        .drop(1)
        .map(l => converterService.toUpdateApiConcept(concept, l))

      newConcept(newCon, mainNodeId) match {
        case Success(c) =>
          val (failed, _) =
            updateCons.map(u => updateConcept(u, c.id)).partition(_.isFailure)
          if (failed.nonEmpty) {
            val failedMsgs = failed.map(_.failed.get.getMessage).mkString(", ")
            Failure(ImportException(s"${c.id}", s"Failed to update one or more concepts: $failedMsgs"))
          } else {
            Success(c)
          }
        case Failure(ex) => Failure(ex)
      }
    }

    def newEmptyConcept(mainNodeId: String): Try[Long] =
      post(s"$DraftApiInternEndpoint/empty_concept/", "externalId" -> mainNodeId)

    def updateConcept(concept: api.UpdateConcept, id: Long): Try[api.Concept] = {
      patch[api.Concept, api.UpdateConcept](s"$DraftApiConceptPublicEndpoint/$id", concept)
    }

    def updateConcept(concept: api.UpdateConcept, mainNodeId: String): Try[api.Concept] = {
      getConceptIdFromExternalId(mainNodeId) match {
        case Some(id) => updateConcept(concept, id)
        case None =>
          Failure(NotFoundException(s"No concept with external id $mainNodeId found"))
      }
    }

    def updateConcept(concept: Concept, mainNodeId: String): Try[api.Concept] = {
      val updateCons = concept.supportedLanguages.map(l => converterService.toUpdateApiConcept(concept, l))
      val (failed, updated) =
        updateCons.map(u => updateConcept(u, mainNodeId)).partition(_.isFailure)
      if (failed.nonEmpty) {
        val failedMsg = failed.map(_.failed.get.getMessage).mkString(", ")
        Failure(ImportException(s"${concept.id}", s"Failed to update one or more concept: $failedMsg"))
      } else {
        updated.head
      }
    }

    def publishConcept(id: Long): Try[Long] = {
      post[ContentId](s"$DraftApiInternEndpoint/concept/$id/publish/").map(_.id)
    }

    def deleteConcept(id: Long): Try[ContentId] =
      delete[ContentId](s"$DraftApiInternEndpoint/article/$id/")

    def getConceptIdFromExternalId(externalId: String): Option[Long] = {
      get[ContentId](s"$DraftApiConceptPublicEndpoint/external_id/$externalId")
        .map(_.id)
        .toOption
    }

    private def get[A](endpointUrl: String, params: Seq[(String, String)] = Seq.empty)(
        implicit mf: Manifest[A]): Try[A] = {
      ndlaClient.fetchWithForwardedAuth[A](Http(endpointUrl).params(params))
    }

    private def post[A](endpointUrl: String, params: (String, String)*)(implicit mf: Manifest[A],
                                                                        format: org.json4s.Formats): Try[A] = {
      ndlaClient.fetchWithForwardedAuth[A](Http(endpointUrl).method("POST").params(params.toMap))
    }

    private def postWithData[A, B <: AnyRef](endpointUrl: String, data: B, params: (String, String)*)(
        implicit mf: Manifest[A],
        format: org.json4s.Formats): Try[A] = {
      ndlaClient.fetchWithForwardedAuth[A](
        Http(endpointUrl)
          .postData(write(data))
          .method("POST")
          .header("content-type", "application/json")
          .params(params.toMap)
      )
    }

    private def put[A](endpointUrl: String)(implicit mf: Manifest[A], format: org.json4s.Formats): Try[A] = {
      ndlaClient.fetchWithForwardedAuth[A](Http(endpointUrl).method("PUT"))
    }

    private def patch[A, B <: AnyRef](endpointUrl: String, data: B, params: (String, String)*)(
        implicit mf: Manifest[A],
        format: org.json4s.Formats): Try[A] = {
      ndlaClient.fetchWithForwardedAuth[A](
        Http(endpointUrl)
          .postData(write(data))
          .method("PATCH")
          .header("content-type", "application/json")
          .params(params)
      )
    }

    private def delete[A](endpointUrl: String, params: (String, String)*)(implicit mf: Manifest[A],
                                                                          format: org.json4s.Formats): Try[A] = {
      ndlaClient.fetchWithForwardedAuth[A](Http(endpointUrl).method("DELETE").params(params.toMap))
    }

    def isHealthy: Boolean = {
      Try(Http(DraftHealthEndpoint).execute()) match {
        case Success(resp) => resp.isSuccess
        case _             => false
      }
    }
  }
}

case class ContentId(id: Long)
