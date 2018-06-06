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
import no.ndla.articleimport.ArticleImportProperties.supportedContentTypes

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
          (nid, s"Related content with node node id $nid ($nodeType) is unsupported and will not be imported")
      }

      if (nidsToImport.isEmpty) {
        Success(content,
                importStatus
                  .addMessages(excludedNids.map(_._2).toSeq))
      } else {
        val importRelatedContentCb: (Set[String], ImportStatus) => Try[(Set[Long], ImportStatus)] =
          importRelatedContent(content.nid, _, _)
        val handlerFunc =
          if (importStatus.nodeLocalContext.depth > 1) getRelatedContentFromDb _
          else importRelatedContentCb

        handlerFunc(nidsToImport, importStatus) match {
          case Success((ids, status)) if ids.nonEmpty =>
            val element = stringToJsoupDocument(content.content)
            element.append(s"<section>${HtmlTagGenerator.buildRelatedContent(ids)}</section>")
            Success(content.copy(content = jsoupDocumentToString(element)),
                    status.addMessages(excludedNids.map(_._2).toSeq))
          case Success((_, status)) =>
            Success(content, status.addMessages(excludedNids.map(_._2).toSeq))
          case Failure(ex: ImportExceptions) =>
            val filteredOutNodes = excludedNids.map {
              case (_, message) =>
                ImportException(content.nid, message, Some(ex))
            }
            Failure(ex.copy(errors = ex.errors ++ filteredOutNodes))
          case Failure(ex) => Failure(ex)
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
      Success(ids, updatedStatus)
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
