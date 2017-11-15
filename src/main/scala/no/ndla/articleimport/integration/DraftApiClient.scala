/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.integration

import no.ndla.articleimport.ArticleImportProperties
import no.ndla.articleimport.model.domain.{Article, Concept}
import no.ndla.network.NdlaClient

import scala.util.{Success, Try}
import scalaj.http.Http

trait DraftApiClient {
  this: NdlaClient =>
  val draftApiClient: DraftApiClient

  class DraftApiClient {
    private val DraftMetaInternEndpoint = s"http://${ArticleImportProperties.DraftHost}/intern"
    private val DraftMetaFromExternalIdEndpoint = s"$DraftMetaInternEndpoint/:external_id"
    private val ImportAudioEndpoint = s"$DraftMetaInternEndpoint/import/:external_id"
    private val DraftHealthEndpoint = s"http://${ArticleImportProperties.DraftHost}/health"

    def getContentByExternalId(externalId: String): Option[Article] = throw new NotImplementedError()

    def newArticle(article: Article, mainNodeId: String, subjectIds: Set[String]): Try[Article] = throw new NotImplementedError()

    def newEmptyArticle(mainNodeId: String, subjectIds: Set[String]): Try[Long] = throw new NotImplementedError()

    def updateArticle(article: Article, mainNodeId: String, forceUpdate: Boolean): Try[Article] = throw new NotImplementedError()

    def setArticleStatus(id: Long, status: Set[String]): Try[_] = throw new NotImplementedError()

    def publishArticle(id: Long): Try[Long] = throw new NotImplementedError()

    def getArticleIdFromExternalId(externalId: String): Option[Long] = throw new NotImplementedError()

    def deleteArticle(id: Long): Try[_] =  throw new NotImplementedError()


    def newOrUpdateConcept(externalId: String, article: Article): Try[Article] = throw new NotImplementedError()

    def newConcept(article: Concept, mainNodeId: String): Try[Concept] = throw new NotImplementedError()

    def newEmptyConcept(mainNodeId: String): Try[Long] = throw new NotImplementedError()

    def updateConcept(article: Concept, mainNodeId: String, forceUpdate: Boolean): Try[Concept] = throw new NotImplementedError()

    def setConceptStatus(id: Long, status: Set[String]): Try[_] = throw new NotImplementedError()

    def publishConcept(id: Long): Try[_] = throw new NotImplementedError()

    def getConceptIdFromExternalId(externalId: String): Option[Long] = throw new NotImplementedError()

    def deleteConcept(id: Long): Try[_] =  throw new NotImplementedError()

    def isHealthy: Boolean = {
      Try(Http(DraftHealthEndpoint).execute()) match {
        case Success(resp) => resp.isSuccess
        case _ => false
      }
    }
  }
}
