/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters

import com.typesafe.scalalogging.LazyLogging
import no.ndla.articleimport.integration.ConverterModule.{jsoupDocumentToString, stringToJsoupDocument}
import no.ndla.articleimport.integration._
import no.ndla.articleimport.model.api.{Article, Concept, ImportException, ImportExceptions}
import no.ndla.articleimport.model.domain.{ExternalEmbedMetaWithTitle, ImportStatus, Language}
import no.ndla.articleimport.service.{ExtractConvertStoreContent, ExtractService}
import no.ndla.articleimport.ArticleImportProperties.{nodeTypeLink, supportedContentTypes}
import no.ndla.validation.TagAttributes._
import no.ndla.validation.ResourceType
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.jsoup.nodes.Entities.EscapeMode

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
      val allRelatedNids =
        content.relatedContent.map(c => (c.nid, extractService.getNodeType(c.nid).getOrElse("unknown"))).toSet

      val nidsToImportWithType = allRelatedNids.filter {
        case (_, nodeType) => supportedContentTypes.contains(nodeType)
      }

      val linkNodesWithOnlyUrl = getLinkNodesWithoutEmbed(nidsToImportWithType, content.language)
      val nidsToImport = nidsToImportWithType.map { case (nid, _) => nid }.diff(linkNodesWithOnlyUrl.map(_.nid))

      val excludedNids = allRelatedNids.filterNot {
        case (nid, _) => nidsToImport.contains(nid) || linkNodesWithOnlyUrl.map(_.nid).contains(nid)
      } map {
        case (nid, nodeType) =>
          (nid, s"Related content with node node id $nid ($nodeType) is unsupported and will not be imported")
      }

      val importRelatedContentCb: (Set[String], ImportStatus) => Try[(Set[Long], ImportStatus)] =
        importRelatedContent(content.nid, _, _)
      val importFunction =
        if (importStatus.importRelatedArticles) importRelatedContentCb
        else getRelatedContentFromDb _

      val statusWithExcluded =
        importStatus.addMessages(excludedNids.map(_._2).toSeq).copy(importRelatedArticles = false)

      if (nidsToImport.isEmpty && linkNodesWithOnlyUrl.isEmpty) {
        Success(content, statusWithExcluded)
      } else {
        importFunction(nidsToImport, statusWithExcluded) match {
          case Success((ids, status)) if ids.nonEmpty || linkNodesWithOnlyUrl.nonEmpty =>
            val embedDiv = HtmlTagGenerator.buildRelatedContent(ids.toList, linkNodesWithOnlyUrl.toList)
            val element = stringToJsoupDocument(content.content)
            element.appendElement("section").appendChild(embedDiv)

            Success(content.copy(content = jsoupDocumentToString(element)), status)
          case Success((_, status)) =>
            Success(content, status)
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

    /**
      * @param nidsAndType
      * @param language
      * @return Returns set of nid, title and url for linknodes without embed codes listed in nidsAndType
      */
    private def getLinkNodesWithoutEmbed(nidsAndType: Set[(String, String)],
                                         language: String): Set[ExternalEmbedMetaWithTitle] = {
      nidsAndType.filter(_._2 == nodeTypeLink).flatMap {
        case (nid, _) =>
          extractService.getLinkEmbedMeta(nid) match {
            case Success(MigrationEmbedMeta(Some(url), None)) =>
              extractService.getNodeData(nid) match {
                case Success(node) =>
                  val linkTitle =
                    Language.findByLanguageOrBestEffort(node.titles, language).map(_.title).getOrElse("")
                  Some(ExternalEmbedMetaWithTitle(nid, linkTitle, url))
                case _ => None
              }
            case _ => None
          }
      }
    }
  }

  private def importRelatedContent(mainNodeId: String,
                                   relatedNids: Set[String],
                                   importStatus: ImportStatus): Try[(Set[Long], ImportStatus)] = {
    val (importedArticles, updatedStatus) =
      relatedNids.foldLeft((Seq[Try[Article]](), importStatus.copy(importRelatedArticles = false)))((result, nid) => {
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
