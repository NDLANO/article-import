/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.integration

import no.ndla.articleimport.ArticleImportProperties
import no.ndla.articleimport.model.api.{NewArticle, NewConcept, NotFoundException}
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
    private val DraftApiInternEndpoint = s"http://${ArticleImportProperties.DraftHost}/intern"
    private val DraftApiPublicEndpoint = s"http://${ArticleImportProperties.DraftHost}/draft-api/v1/drafts"
    private val DraftApiConceptPublicEndpoint = s"http://${ArticleImportProperties.DraftHost}/draft-api/v1/concepts"
    private val DraftIdFromExternalIdEndpoint = s"$DraftApiInternEndpoint/external_id/:nid"
    private val ImportAudioEndpoint = s"$DraftApiInternEndpoint/import/:external_id"
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
      implicit val formats = org.json4s.DefaultFormats
      post[api.Article, NewArticle](s"$DraftApiPublicEndpoint/", article, Seq("externalId" -> mainNodeId))
    }

    def newArticle(article: Article, mainNodeId: String, subjectIds: Set[String]): Try[api.Article] = {
      implicit val formats = org.json4s.DefaultFormats
      val newArt = converterService.toApiNewArticle(article, article.supportedLanguages.head)
      val updateArticles = article.supportedLanguages.drop(1).zipWithIndex
          .map { case (lang, idx) => converterService.toApiUpdateArticle(article, lang, idx + 1)}

      for {
        a <- newArticle(newArt, mainNodeId, subjectIds)
        updatedArticle <- updateArticles
        _ <- updateArticle(updatedArticle, a.id, forceUpdate = true)
      } yield a
    }

    def newEmptyArticle(mainNodeId: String, subjectIds: Set[String]): Try[Long] = post(s"$DraftApiInternEndpoint/empty_article", "")

    def updateArticle(article: api.UpdateArticle, id: Long, forceUpdate: Boolean): Try[api.Article] = {
      implicit val formats = org.json4s.DefaultFormats
      patch[api.Article, api.UpdateArticle](s"$DraftApiPublicEndpoint/$id", article)
    }

    def updateArticle(article: api.UpdateArticle, mainNodeId: String, forceUpdate: Boolean): Try[api.Article] = {
      getArticleIdFromExternalId(mainNodeId) match {
        case Some(id) => updateArticle(article, id, forceUpdate = true)
        case None => Failure(NotFoundException(s"No article with external id $mainNodeId found"))
      }
    }

    def setArticleStatus(id: Long, status: Set[String]): Try[_] = throw new NotImplementedError()

    def publishArticle(id: Long): Try[Long] = post(s"$DraftApiInternEndpoint/article/$id/publish", "")

    def deleteArticle(id: Long): Try[_] = throw new NotImplementedError()


    def newOrUpdateConcept(externalId: String, article: Article): Try[Article] = throw new NotImplementedError()

    def newConcept(concept: NewConcept, mainNodeId: String): Try[api.Concept] = {
      implicit val formats = org.json4s.DefaultFormats
      post[api.Concept, NewConcept](s"$DraftApiConceptPublicEndpoint/", concept, Seq("externalId" -> mainNodeId))
    }

    def newConcept(concept: Concept, mainNodeId: String): Try[api.Concept] = {
      val newCon: api.NewConcept = converterService.toNewApiConcept(concept, concept.supportedLanguages.headOption.getOrElse(Language.UnknownLanguage))
      val updateCons = concept.supportedLanguages.drop(1).map(l => converterService.toUpdateApiConcept(concept, l))

      for {
        c <- newConcept(newCon, mainNodeId)
        updatedCon <- updateCons
        _ <- updateConcept(updatedCon, c.id, forceUpdate = true)
      } yield c
    }

    def newEmptyConcept(mainNodeId: String): Try[Long] =
      post(s"$DraftApiInternEndpoint/empty_concept", "")

    def updateConcept(concept: api.UpdateConcept, id: Long, forceUpdate: Boolean): Try[api.Concept] = {
      implicit val formats = org.json4s.DefaultFormats
      patch(s"$DraftApiInternEndpoint/$id", concept)
    }

    def updateConcept(concept: api.UpdateConcept, mainNodeId: String, forceUpdate: Boolean): Try[api.Concept] = {
      val a = getConceptIdFromExternalId(mainNodeId) match {
        case Some(id) => updateConcept(concept, id, forceUpdate))
        case None => Failure(NotFoundException(s"No concept with external id $mainNodeId found"))
      }
    }

    def setConceptStatus(id: Long, status: Set[String]): Try[_] = throw new NotImplementedError()

    def publishConcept(id: Long): Try[_] = throw new NotImplementedError()

    def getConceptIdFromExternalId(externalId: String): Option[Long] = {
      get[ContentId](s"$DraftApiConceptPublicEndpoint/external_id/$externalId").map(_.id).toOption
    }

    def deleteConcept(id: Long): Try[_] =  throw new NotImplementedError()

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
