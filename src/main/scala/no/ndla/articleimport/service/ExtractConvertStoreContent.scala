/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.articleimport.service

import com.typesafe.scalalogging.LazyLogging
import no.ndla.articleimport.integration.{DraftApiClient, MigrationApiClient}
import no.ndla.articleimport.model.api.{ImportException, NotFoundException}
import no.ndla.articleimport.model.domain._
import no.ndla.articleimport.ArticleImportProperties.{nodeTypeBegrep, supportedContentTypes}

import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

trait ExtractConvertStoreContent {
  this: ExtractService
    with MigrationApiClient
    with ConverterService
    with DraftApiClient =>

  val extractConvertStoreContent: ExtractConvertStoreContent

  class ExtractConvertStoreContent extends LazyLogging {
    def processNode(externalId: String, forceUpdateArticles: Boolean): Try[(Content, ImportStatus)] =
      processNode(externalId, ImportStatus.empty, forceUpdateArticles)

    def processNode(externalId: String, importStatus: ImportStatus = ImportStatus.empty, forceUpdateArticles: Boolean = false): Try[(Content, ImportStatus)] = {
      if (importStatus.visitedNodes.contains(externalId)) {
        return getMainNodeId(externalId).flatMap(draftApiClient.getContentByExternalId) match {
          case Some(content) => Success(content, importStatus)
          case None => Failure(NotFoundException(s"Content with external id $externalId was not found"))
        }
      }

      val (node, mainNodeId) = extract(externalId) match {
        case Success((n, mnid)) => (n, mnid)
        case Failure(f) => return Failure(f)
      }

      // Generate an ID for the content before converting the node.
      // This ensures that cyclic dependencies between articles does not cause an infinite recursive import job
      generateNewIdIfFirstTimeImported(mainNodeId, node.nodeType)

      val importedArticle = for {
        (convertedContent, updatedImportStatus) <- converterService.toDomainArticle(node, importStatus)
        content <- store(convertedContent, mainNodeId, forceUpdateArticles)
      } yield (content, updatedImportStatus.addMessage(s"Successfully imported node $externalId: ${content.id.get}").setArticleId(content.id.get))

      if (importedArticle.isFailure) {
        deleteContent(externalId, node.nodeType)
      }

      importedArticle
    }

    private def deleteContent(externalId: String, nodeType: String): Unit = {
      nodeType match {
        case `nodeTypeBegrep` => deleteConceptByExternalId(externalId)
        case _ => deleteArticleByExternalId(externalId)
      }
    }

    private def deleteArticleByExternalId(externalId: String) = {
      draftApiClient.getArticleIdFromExternalId(externalId).map(articleId => {
        logger.info(s"Deleting article (id=$articleId, external id=$externalId) from database because the article could not be imported")
        draftApiClient.deleteArticle(articleId)
      })
    }

    private def deleteConceptByExternalId(externalId: String) = {
      draftApiClient.getConceptIdFromExternalId(externalId).map(conceptId => {
        logger.info(s"Deleting concept (id=$conceptId, external id=$externalId) from database because the concept could not be imported")
        draftApiClient.deleteConcept(conceptId)
      })
    }

    def getMainNodeId(externalId: String): Option[String] = {
      extract(externalId) map { case (_, mainNodeId) => mainNodeId } toOption
    }

    private def extract(externalId: String): Try[(NodeToConvert, String)] = {
      val node = extractService.getNodeData(externalId)
      node.contents.find(_.isMainNode) match {
        case None => Failure(NotFoundException(s"$externalId is a translation; Could not find main node"))
        case Some(mainNode) =>
          if (supportedContentTypes.contains(node.nodeType.toLowerCase) || supportedContentTypes.contains(node.contentType.toLowerCase))
            Success(node, mainNode.nid)
          else
            Failure(ImportException(s"Tried to import node of unsupported type '${node.nodeType.toLowerCase}/${node.contentType.toLowerCase()}'"))
      }
    }

    private def store(content: Content, mainNodeId: String, forceUpdateArticle: Boolean): Try[Content] = {
      content match {
        case article: Article => storeArticle(article, mainNodeId, forceUpdateArticle)
        case concept: Concept => storeConcept(concept, mainNodeId, forceUpdateArticle)
      }
    }

    private def storeArticle(article: Article, mainNodeNid: String, forceUpdate: Boolean): Try[Content] = {
      val storedArticle = draftApiClient.getArticleIdFromExternalId(mainNodeNid).isDefined match {
        case true => draftApiClient.updateArticle(article, mainNodeNid, forceUpdate)
        case false => draftApiClient.newArticle(article, mainNodeNid, getSubjectIds(mainNodeNid))
      }

      storedArticle.map(a => draftApiClient.publishArticle(a.id.get)).flatMap(_ => storedArticle)
    }

    private def storeConcept(concept: Concept, mainNodeNid: String, forceUpdate: Boolean): Try[Content] = {
      val storedConcept = draftApiClient.getConceptIdFromExternalId(mainNodeNid).isDefined match {
        case true => draftApiClient.updateConcept(concept, mainNodeNid, forceUpdate)
        case false => draftApiClient.newConcept(concept, mainNodeNid)
      }

      storedConcept.map(a => draftApiClient.publishConcept(a.id.get)).flatMap(_ => storedConcept)
    }

    private def getSubjectIds(nodeId: String): Set[String] =
      migrationApiClient.getSubjectForNode(nodeId) match {
        case Failure(ex) => Set()
        case Success(subjectMetas) => subjectMetas.map(_.nid)
      }

    private def generateNewIdIfFirstTimeImported(nodeId: String, nodeType: String): Option[Long] = {
      nodeType match {
        case `nodeTypeBegrep` => generateNewConceptIdIfExternalIdDoesNotExist(nodeId)
        case _ => generateNewArticleIdIfExternalIdDoesNotExist(nodeId)
      }
    }

    private def generateNewArticleIdIfExternalIdDoesNotExist(nodeId: String): Option[Long] = {
      draftApiClient.getArticleIdFromExternalId(nodeId) match {
        case None => draftApiClient.newEmptyArticle(nodeId, getSubjectIds(nodeId)).toOption
        case Some(id) => Some(id)
      }
    }

    private def generateNewConceptIdIfExternalIdDoesNotExist(externalId: String): Option[Long] = {
      draftApiClient.getConceptIdFromExternalId(externalId) match {
        case None => draftApiClient.newEmptyConcept(externalId).toOption
        case Some(id) => Some(id)
      }
    }

  }
}
