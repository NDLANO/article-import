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
import no.ndla.articleimport.model.api
import no.ndla.articleimport.model.domain._
import no.ndla.articleimport.ArticleImportProperties.{nodeTypeBegrep, supportedContentTypes}

import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

trait ExtractConvertStoreContent {
  this: ExtractService with MigrationApiClient with ConverterService with DraftApiClient =>

  val extractConvertStoreContent: ExtractConvertStoreContent

  class ExtractConvertStoreContent extends LazyLogging {

    def processNode(externalId: String, importStatus: ImportStatus): Try[(ApiContent, ImportStatus)] = {
      if (importStatus.visitedNodes.contains(externalId)) {
        return getMainNodeId(externalId).flatMap(draftApiClient.getContentByExternalId) match {
          case Some(content) => Success(content, importStatus)
          case None =>
            Failure(NotFoundException(s"Content with external id $externalId was not found"))
        }
      }

      val (node, mainNodeId) = extract(externalId) match {
        case Success((n, mnid)) => (n, mnid)
        case Failure(f)         => return Failure(f)
      }

      val convertedNode = for {
        // Generate an ID for the content before converting the node.
        // This ensures that cyclic dependencies between articles does not cause an infinite recursive import job
        _ <- generateNewIdIfFirstTimeImported(mainNodeId, node.nodeType)
        (convertedContent, updatedImportStatus) <- converterService
          .toDomainArticle(node, importStatus.withNewNodeLocalContext())
        (content, storeImportStatus) <- store(convertedContent, mainNodeId, updatedImportStatus)
      } yield
        (content,
         storeImportStatus
           .addMessage(s"Successfully imported node $externalId: ${content.id}")
           .setArticleId(content.id)
           .resetNodeLocalContext(importStatus.nodeLocalContext))

      convertedNode match {
        case Success(converted) => Success(converted)
        case Failure(ex) =>
          logger.warn(s"Failed to import node with id $externalId. Deleting any previous version")
          deleteContent(mainNodeId, node.nodeType)
            .foreach(id => logger.warn(s"Deleted content with id $id with node type ${node.nodeType}"))
          Failure(ex)
      }
    }

    def getMainNodeId(externalId: String): Option[String] = {
      extract(externalId) map { case (_, mainNodeId) => mainNodeId } toOption
    }

    private def deleteContent(mainNodeId: String, nodeType: String): Option[Long] = {
      nodeType match {
        case `nodeTypeBegrep` =>
          draftApiClient
            .getConceptIdFromExternalId(mainNodeId)
            .flatMap(draftApiClient.deleteConcept(_).toOption)
            .map(_.id)
        case _ =>
          draftApiClient
            .getArticleIdFromExternalId(mainNodeId)
            .flatMap(draftApiClient.getArticleFromId)
            .filter(article => article.revision.getOrElse(1) <= 1) // don't delete article if it has been manually edited
            .flatMap(article => draftApiClient.deleteArticle(article.id).toOption)
            .map(_.id)
      }
    }

    private def extract(externalId: String): Try[(NodeToConvert, String)] = {
      val node = extractService.getNodeData(externalId)
      val nodeType = node.map(_.nodeType).getOrElse("")
      val contentType = node.map(_.contentType).getOrElse("")

      node.map(_.contents.find(_.isMainNode)) match {
        case Success(None) =>
          Failure(NotFoundException(s"$externalId is a translation; Could not find main node"))
        case Success(Some(mainNode)) =>
          if (supportedContentTypes.contains(nodeType.toLowerCase) || supportedContentTypes
                .contains(contentType.toLowerCase))
            node.map(n => (n, mainNode.nid))
          else
            Failure(
              ImportException(externalId,
                              s"Tried to import node of unsupported type '${nodeType.toLowerCase}/${contentType
                                .toLowerCase()}'"))
        case Failure(ex) => Failure(ex)
      }
    }

    private def store(content: Content,
                      mainNodeId: String,
                      importStatus: ImportStatus): Try[(ApiContent, ImportStatus)] = {
      content match {
        case article: Article => storeArticle(article, mainNodeId, importStatus)
        case concept: Concept =>
          storeConcept(concept, mainNodeId) match {
            case Success(con) => Success((con, importStatus))
            case Failure(ex)  => Failure(ex)
          }
      }
    }

    private[service] def storeArticle(article: Article,
                                      mainNodeNid: String,
                                      importStatus: ImportStatus): Try[(ApiContent, ImportStatus)] = {
      draftApiClient.getArticleIdFromExternalId(mainNodeNid) match {
        case Some(id) =>
          draftApiClient.getArticleFromId(id) match {
            case Some(content) if content.revision.getOrElse(1) > 1 =>
              content match {
                case storedArticle: api.Article =>
                  if (importStatus.forceUpdateArticles) {
                    logger.info("forceUpdateArticles is set, updating anyway...")
                    val storeImportStatus = importStatus.addMessage(
                      s"$mainNodeNid has been updated since import, but forceUpdateArticles is set, updating anyway")
                    draftApiClient
                      .updateArticle(article, mainNodeNid, getSubjectIds(mainNodeNid))
                      .flatMap(publishArticle(_, storeImportStatus))
                  } else {
                    logger.info("Article has been updated since import, refusing to import...")
                    val storeImportStatus =
                      importStatus.addMessage(s"$mainNodeNid has been updated since import, refusing to import.")
                    Success(storedArticle, storeImportStatus)
                  }
                case _ =>
                  logger.error("ApiContent of storeArticle was not an article. This is a bug.")
                  Failure(
                    ImportException(
                      mainNodeNid,
                      s"Remote type of $mainNodeNid was not article, yet was imported as one. This is a bug."))
              }
            case _ =>
              draftApiClient
                .updateArticle(article, mainNodeNid, getSubjectIds(mainNodeNid))
                .flatMap(publishArticle(_, importStatus))
          }
        case _ =>
          draftApiClient
            .newArticle(article, mainNodeNid, getSubjectIds(mainNodeNid))
            .flatMap(publishArticle(_, importStatus))

      }
    }

    private def publishArticle(storedArticle: ApiContent,
                               importStatus: ImportStatus): Try[(ApiContent, ImportStatus)] = {
      draftApiClient.publishArticle(storedArticle.id) match {
        case Success(status) if Set("PUBLISHED", "IMPORTED").subsetOf(status.status) =>
          Success(storedArticle, importStatus)
        case Success(status) =>
          Failure(
            ImportException(
              s"${storedArticle.id}",
              s"Published article does not contain expected statuses PUBLISHED and IMPORTED (${status.status})"))
        case Failure(ex) => Failure(ex)
      }
    }

    private def storeConcept(concept: Concept, mainNodeNid: String): Try[ApiContent] = {
      val storedConcept =
        draftApiClient.getConceptIdFromExternalId(mainNodeNid).isDefined match {
          case true  => draftApiClient.updateConcept(concept, mainNodeNid)
          case false => draftApiClient.newConcept(concept, mainNodeNid)
        }

      storedConcept
        .flatMap(c => draftApiClient.publishConcept(c.id))
        .flatMap(_ => storedConcept)
    }

    private def getSubjectIds(nodeId: String): Set[String] =
      migrationApiClient.getSubjectForNode(nodeId) match {
        case Failure(ex)           => Set()
        case Success(subjectMetas) => subjectMetas.map(_.nid)
      }

    private def generateNewIdIfFirstTimeImported(nodeId: String, nodeType: String): Try[Long] = {
      nodeType match {
        case `nodeTypeBegrep` =>
          generateNewConceptIdIfExternalIdDoesNotExist(nodeId)
        case _ => generateNewArticleIdIfExternalIdDoesNotExist(nodeId)
      }
    }

    private def generateNewArticleIdIfExternalIdDoesNotExist(nodeId: String): Try[Long] = {
      draftApiClient.getArticleIdFromExternalId(nodeId) match {
        case None =>
          draftApiClient
            .newEmptyArticle(nodeId, getSubjectIds(nodeId))
            .map(_.id)
        case Some(id) => Success(id)
      }
    }

    private def generateNewConceptIdIfExternalIdDoesNotExist(externalId: String): Try[Long] = {
      draftApiClient.getConceptIdFromExternalId(externalId) match {
        case None     => draftApiClient.newEmptyConcept(externalId)
        case Some(id) => Success(id)
      }
    }

  }

}
