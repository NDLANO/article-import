/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.articleimport.service

import com.typesafe.scalalogging.LazyLogging
import no.ndla.articleimport.integration.{DraftApiClient, MigrationApiClient}
import no.ndla.articleimport.model.api.{ApiContent, ImportException, NotFoundException}
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
    def processNode(externalId: String): Try[(ApiContent, ImportStatus)] =
      processNode(externalId, ImportStatus.empty)

    def processNode(externalId: String, importStatus: ImportStatus = ImportStatus.empty, forceUpdateArticles: Boolean = false): Try[(ApiContent, ImportStatus)] = {
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

      for {
        (convertedContent, updatedImportStatus) <- converterService.toDomainArticle(node, importStatus)
        content <- store(convertedContent, mainNodeId)
      } yield (content, updatedImportStatus.addMessage(s"Successfully imported node $externalId: ${content.id}").setArticleId(content.id))
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

    private def store(content: Content, mainNodeId: String): Try[ApiContent] = {
      content match {
        case article: Article => storeArticle(article, mainNodeId)
        case concept: Concept => storeConcept(concept, mainNodeId)
      }
    }

    private def storeArticle(article: Article, mainNodeNid: String): Try[ApiContent] = {
      val storedArticle = draftApiClient.getArticleIdFromExternalId(mainNodeNid).isDefined match {
        case true => draftApiClient.updateArticle(article, mainNodeNid)
        case false => draftApiClient.newArticle(article, mainNodeNid, getSubjectIds(mainNodeNid))
      }

      storedArticle.flatMap(a => draftApiClient.publishArticle(a.id)).flatMap(_ => storedArticle)
    }

    private def storeConcept(concept: Concept, mainNodeNid: String): Try[ApiContent] = {
      val storedConcept = draftApiClient.getConceptIdFromExternalId(mainNodeNid).isDefined match {
        case true => draftApiClient.updateConcept(concept, mainNodeNid)
        case false => draftApiClient.newConcept(concept, mainNodeNid)
      }

      storedConcept.flatMap(c => draftApiClient.publishConcept(c.id)).flatMap(_ => storedConcept)
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
