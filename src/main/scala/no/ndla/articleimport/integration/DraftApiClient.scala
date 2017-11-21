/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.integration

import no.ndla.articleimport.ArticleImportProperties
import no.ndla.articleimport.model.api.{ImportException, NewArticle, NewConcept, NotFoundException}
import no.ndla.articleimport.model.domain.{Article, Concept, Language}
import no.ndla.network.NdlaClient
import no.ndla.articleimport.model.api
import no.ndla.articleimport.service.ConverterService

import scala.util.{Failure, Success, Try}
import scalaj.http.Http
import org.json4s.native.Serialization.write

trait DraftApiClient {
  this: NdlaClient with ConverterService =>
  val draftApiClient: DraftApiClient

  class DraftApiClient {
    implicit val formats = org.json4s.DefaultFormats

    private val DraftApiInternEndpoint = s"http://${ArticleImportProperties.DraftHost}/intern"
    private val DraftApiPublicEndpoint = s"http://${ArticleImportProperties.DraftHost}/draft-api/v1/drafts"
    private val DraftApiConceptPublicEndpoint = s"http://${ArticleImportProperties.DraftHost}/draft-api/v1/concepts"
    private val DraftHealthEndpoint = s"http://${ArticleImportProperties.DraftHost}/health"

    def getContentByExternalId(externalId: String): Option[Article] = {
      getArticleIdFromExternalId(externalId).flatMap(getArticleFromId)
    }

    def getArticleFromId(id: Long): Option[Article] = {
      get[Article](s"$DraftApiPublicEndpoint/$id").toOption
    }

    def getArticleIdFromExternalId(externalId: String): Option[Long] = {
      get[ContentId](s"$DraftApiPublicEndpoint/external_id/$externalId").map(_.id).toOption
    }

    def newArticle(article: NewArticle, mainNodeId: String, subjectIds: Set[String]): Try[api.Article] = {
      post[api.Article, NewArticle](s"$DraftApiPublicEndpoint/", article, Seq("externalId" -> mainNodeId))
    }

    def newArticle(article: Article, mainNodeId: String, subjectIds: Set[String]): Try[api.Article] = {
      val newArt = converterService.toApiNewArticle(article, article.supportedLanguages.head)
      val updateArticles = article.supportedLanguages.drop(1).zipWithIndex
          .map { case (lang, idx) => converterService.toApiUpdateArticle(article, lang, idx + 1)}

      newArticle(newArt, mainNodeId, subjectIds) match {
        case Success(a) =>
          val (failed, _) = updateArticles.map(u => updateArticle(u, a.id, forceUpdate = true)).partition(_.isFailure)
          if (failed.nonEmpty) {
            val failedMsgs = failed.map(_.failed.get.getMessage).mkString(", ")
            Failure(ImportException(s"Failed to update one or more article: $failedMsgs"))
          } else {
            Success(a)
          }
        case Failure(ex) => Failure(ex)
      }
    }

    def newEmptyArticle(mainNodeId: String, subjectIds: Set[String]): Try[Long] = post(s"$DraftApiInternEndpoint/empty_article", "")

    def updateArticle(article: api.UpdateArticle, id: Long, forceUpdate: Boolean): Try[api.Article] = {
      patch[api.Article, api.UpdateArticle](s"$DraftApiPublicEndpoint/$id", article)
    }

    def updateArticle(article: api.UpdateArticle, mainNodeId: String, forceUpdate: Boolean): Try[api.Article] = {
      getArticleIdFromExternalId(mainNodeId) match {
        case Some(id) => updateArticle(article, id, forceUpdate = true)
        case None => Failure(NotFoundException(s"No article with external id $mainNodeId found"))
      }
    }

    def updateArticle(article: Article, mainNodeId: String, forceUpdate: Boolean): Try[api.Article] = {
      val updateArticles = article.supportedLanguages.zipWithIndex
        .map { case (lang, idx) => converterService.toApiUpdateArticle(article, lang, idx + 1)}

      val (failed, updated) = updateArticles.map(u => updateArticle(u, mainNodeId, forceUpdate)).partition(_.isFailure)
      if (failed.nonEmpty) {
        val failedMsg = failed.map(_.failed.get.getMessage).mkString(", ")
        Failure(ImportException(s"Failed to update one or more article: $failedMsg"))
      } else {
        updated.head
      }
    }

    def publishArticle(id: Long): Try[Long] = post(s"$DraftApiInternEndpoint/article/$id/publish", "")

    def deleteArticle(id: Long): Try[_] = post(s"$DraftApiPublicEndpoint/article/$id/delete", "")


    def newConcept(concept: NewConcept, mainNodeId: String): Try[api.Concept] = {
      post[api.Concept, NewConcept](s"$DraftApiConceptPublicEndpoint/", concept, Seq("externalId" -> mainNodeId))
    }

    def newConcept(concept: Concept, mainNodeId: String): Try[api.Concept] = {
      val newCon: api.NewConcept = converterService.toNewApiConcept(concept, concept.supportedLanguages.headOption.getOrElse(Language.UnknownLanguage))
      val updateCons = concept.supportedLanguages.drop(1).map(l => converterService.toUpdateApiConcept(concept, l))

      newConcept(newCon, mainNodeId) match {
        case Success(c) =>
          val (failed, _) = updateCons.map(u => updateConcept(u, c.id, forceUpdate = true)).partition(_.isFailure)
          if (failed.nonEmpty) {
            val failedMsgs = failed.map(_.failed.get.getMessage).mkString(", ")
            Failure(ImportException(s"Failed to update one or more article: $failedMsgs"))
          } else {
            Success(c)
          }
        case Failure(ex) => Failure(ex)
      }
    }

    def newEmptyConcept(mainNodeId: String): Try[Long] =
      post(s"$DraftApiInternEndpoint/empty_concept", "")

    def updateConcept(concept: api.UpdateConcept, id: Long, forceUpdate: Boolean): Try[api.Concept] = {
      patch(s"$DraftApiInternEndpoint/$id", concept)
    }

    def updateConcept(concept: api.UpdateConcept, mainNodeId: String, forceUpdate: Boolean): Try[api.Concept] = {
      getConceptIdFromExternalId(mainNodeId) match {
        case Some(id) => updateConcept(concept, id, forceUpdate)
        case None => Failure(NotFoundException(s"No concept with external id $mainNodeId found"))
      }
    }

    def updateConcept(concept: Concept, mainNodeId: String, forceUpdate: Boolean): Try[api.Concept] = {
      val updateCons = concept.supportedLanguages.map(l => converterService.toUpdateApiConcept(concept, l))
      val (failed, updated) = updateCons.map(u => updateConcept(u, mainNodeId, forceUpdate)).partition(_.isFailure)
      if (failed.nonEmpty) {
        val failedMsg = failed.map(_.failed.get.getMessage).mkString(", ")
        Failure(ImportException(s"Failed to update one or more article: $failedMsg"))
      } else {
        updated.head
      }
    }

    def publishConcept(id: Long): Try[Long] = post(s"$DraftApiInternEndpoint/$id/publish", "")

    def getConceptIdFromExternalId(externalId: String): Option[Long] = {
      get[ContentId](s"$DraftApiConceptPublicEndpoint/external_id/$externalId").map(_.id).toOption
    }

    def deleteConcept(id: Long): Try[_] = post(s"$DraftApiConceptPublicEndpoint/$id/delete", "")

    private def get[A](endpointUrl: String, params: Seq[(String, String)] = Seq.empty)(implicit mf: Manifest[A]): Try[A] = {
      ndlaClient.fetch[A](Http(endpointUrl).params(params))
    }

    private def post[A, B <: AnyRef](endpointUrl: String, data: B, params: Seq[(String, String)] = Seq.empty)(implicit mf: Manifest[A], format: org.json4s.Formats): Try[A] = {
      ndlaClient.fetch[A](
        Http(endpointUrl)
          .postData(write(data))
          .method("POST")
          .header("content-type", "application/json")
      )
    }

    private def patch[A, B <: AnyRef](endpointUrl: String, data: B, params: Seq[(String, String)] = Seq.empty)(implicit mf: Manifest[A], format: org.json4s.Formats): Try[A] = {
      ndlaClient.fetch[A](
        Http(endpointUrl)
          .postData(write(data))
          .method("PATCH")
          .header("content-type", "application/json")
      )
    }

    def isHealthy: Boolean = {
      Try(Http(DraftHealthEndpoint).execute()) match {
        case Success(resp) => resp.isSuccess
        case _ => false
      }
    }
  }
}

case class ContentId(id: Long)
