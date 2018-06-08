/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters

import com.typesafe.scalalogging.LazyLogging
import no.ndla.articleimport.integration.ConverterModule.{jsoupDocumentToString, stringToJsoupDocument}
import no.ndla.articleimport.integration.{ConverterModule, DraftApiClient, LanguageContent, MigrationApiClient}
import no.ndla.articleimport.model.api.{Article, Concept, ImportException, ImportExceptions}
import no.ndla.articleimport.model.domain.ImportStatus
import no.ndla.articleimport.service.{ExtractConvertStoreContent, ExtractService}
import no.ndla.articleimport.ArticleImportProperties.{supportedContentTypes, importRelatedNodesMaxDepth}

import scala.util.{Failure, Success, Try}

trait RelatedContentConverter {
  this: ExtractConvertStoreContent
    with HtmlTagGenerator
    with MigrationApiClient
    with LazyLogging
    with ExtractService
    with DraftApiClient =>

  object RelatedContentConverter extends ConverterModule {
    override def convert(content: LanguageContent, importStatus: ImportStatus): Try[(LanguageContent, ImportStatus)] = {
      val allRelatedNids = content.relatedContent
        .map(c => (c.nid, extractService.getNodeType(c.nid).getOrElse("unknown")))
        .toSet

      val nidsToImport = allRelatedNids.collect {
        case (nid, nodeType) if supportedContentTypes.contains(nodeType) => nid
      }

      val excludedNids = allRelatedNids.collect {
        case (nid, nodeType) if !nidsToImport.contains(nid) =>
          ImportException(nid,
                          s"Related content with node node id $nid ($nodeType) is unsupported and will not be imported")
      }

      val updatedStatus = importStatus.addErrors(excludedNids.toSeq)

      if (nidsToImport.isEmpty) {
        Success(content, updatedStatus)
      } else {
        val importRelatedContentCb: (Set[String], ImportStatus) => Try[(Set[Long], ImportStatus)] =
          importRelatedContent(content.nid, _, _)
        val handlerFunc =
          if (updatedStatus.nodeLocalContext.depth > importRelatedNodesMaxDepth) getRelatedContentFromDb _
          else importRelatedContentCb

        handlerFunc(nidsToImport, updatedStatus) match {
          case Success((ids, status)) if ids.nonEmpty =>
            val element = stringToJsoupDocument(content.content)
            element.append(s"<section>${HtmlTagGenerator.buildRelatedContent(ids)}</section>")
            Success(content.copy(content = jsoupDocumentToString(element)), status)
          case Success((_, status)) =>
            Success(content, status)
          case Failure(ex: ImportExceptions) =>
            val importExceptions = ex.errors.collect { case ex: ImportException => ex }
            val otherExceptions = ex.errors.diff(importExceptions)
            val finalStatus = updatedStatus
              .addErrors(importExceptions)
              .addErrors(
                otherExceptions.map(
                  e =>
                    ImportException(content.nid,
                                    "Something unexpected went wrong while importing related nodes.",
                                    Some(e))))
            Success((content, finalStatus))
          case Failure(ex) =>
            Success((content, updatedStatus.addError(ImportException(content.nid, ex.getMessage, Some(ex)))))
        }
      }

    }
  }

  private def importRelatedContent(mainNodeId: String,
                                   relatedNids: Set[String],
                                   importStatus: ImportStatus): Try[(Set[Long], ImportStatus)] = {
    val (importedArticles, updatedStatus) =
      relatedNids.foldLeft((Seq[Try[Article]](), importStatus))((result, nid) => {
        val (articles, status) = result

        extractConvertStoreContent.processNode(nid, status) match {
          case Success((content: Article, st)) =>
            (articles :+ Success(content), st)
          case Success((_: Concept, _)) =>
            (articles :+ Failure(
               ImportException(mainNodeId,
                               s"Related content with nid $nid points to a concept. This should not be legal, no?")),
             status)
          case Failure(ex) =>
            (articles :+ Failure(
               ImportException(mainNodeId, s"Failed to import related content with nid $nid", Some(ex))),
             status)
        }
      })

    val (importSuccesses, importFailures) =
      importedArticles.partition(_.isSuccess)
    if (importFailures.isEmpty) {
      val ids = importSuccesses.map(_.get.id).toSet
      Success(ids, updatedStatus) // TODO: If one of three fails, we want to disaply the two imported.
    } else {
      val nodeIds = migrationApiClient
        .getAllTranslationNids(mainNodeId)
        .getOrElse(Set(mainNodeId))
      logger.info(s"Failed to import one or more related contents for node(s) ${nodeIds.mkString(",")}")
      Failure(ImportExceptions(nodeIds, importFailures.map(_.failed.get)))
    }
  }

  private def getRelatedContentFromDb(nids: Set[String], importStatus: ImportStatus): Try[(Set[Long], ImportStatus)] = {
    val ids = nids.flatMap(draftApiClient.getArticleIdFromExternalId)
    Success((ids, importStatus))
  }

}
