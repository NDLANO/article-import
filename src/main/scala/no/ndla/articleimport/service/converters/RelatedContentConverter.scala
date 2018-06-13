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
import no.ndla.articleimport.model.api._
import no.ndla.articleimport.model.domain.ImportStatus
import no.ndla.articleimport.service.{ExtractConvertStoreContent, ExtractService}
import no.ndla.articleimport.ArticleImportProperties.{importRelatedNodesMaxDepth, supportedContentTypes}
import cats.implicits._
import scala.util.{Failure, Success, Try}

trait RelatedContentConverter {
  this: ExtractConvertStoreContent
    with HtmlTagGenerator
    with MigrationApiClient
    with LazyLogging
    with ExtractService
    with TaxonomyApiClient
    with DraftApiClient =>

  object RelatedContentConverter extends ConverterModule {
    override def convert(content: LanguageContent, importStatus: ImportStatus): Try[(LanguageContent, ImportStatus)] = {
      val allRelatedNids = content.relatedContent
        .map(c => (c.nid, extractService.getNodeType(c.nid).getOrElse("unknown")))
        .toSet

      val (excludedNids, nidsToImport) = getValidNids(allRelatedNids.toList).separate

      val updatedStatus = importStatus.addErrors(excludedNids)
      if (nidsToImport.isEmpty) {
        Success(content, updatedStatus)
      } else {
        val importRelatedContentCb: (Set[String], ImportStatus) => Try[(Set[Long], ImportStatus)] =
          importRelatedContent(content.nid, _, _)
        val handlerFunc =
          if (updatedStatus.nodeLocalContext.depth > importRelatedNodesMaxDepth) getRelatedContentFromDb _
          else importRelatedContentCb

        handlerFunc(nidsToImport.toSet, updatedStatus) match {
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
      relatedNids.foldLeft((List[Either[Throwable, Article]](), importStatus))((result, nid) => {
        val (articles, status) = result

        extractConvertStoreContent.processNode(nid, status) match {
          case Success((content: Article, st)) =>
            (articles :+ Right(content), st)
          case Success((_: Concept, _)) =>
            val msg = s"Related content with nid $nid points to a concept. This should not be legal, no?"
            logger.error(msg)
            (articles :+ Left(ImportException(nid, msg)), status)
          case Failure(ex) =>
            val msg = s"Failed to import related content with nid $nid"
            logger.error(msg)
            (articles :+ Left(ImportException(nid, msg, Some(ex))), status)
        }
      })

    val (importFailures, importSuccesses) = importedArticles.separate

    val errors = importFailures.map {
      case fail: ImportException => fail
      case fail                  => ImportException(mainNodeId, "Something went wrong when importing related content", Some(fail))
    }
    val finalStatus = updatedStatus.addErrors(errors)

    Success((importSuccesses.map(_.id).toSet, finalStatus))
  }

  private def getRelatedContentFromDb(nids: Set[String], importStatus: ImportStatus): Try[(Set[Long], ImportStatus)] = {
    val ids = nids.flatMap(draftApiClient.getArticleIdFromExternalId)
    Success((ids, importStatus))
  }

  /**
    * Returns which @nids that are valid
    * @param nids All related nids
    * @return Eithers with ImportException in left and valid nid in right.
    */
  private def getValidNids(nids: List[(String, String)]): List[Either[ImportException, String]] = {
    nids.map {
      case (nid, nodeType) if !supportedContentTypes.contains(nodeType) =>
        Left(
          ImportException(nid,
                          s"Related content with node id $nid ($nodeType) is unsupported and will not be imported."))
      case (nid, _) =>
        taxonomyApiClient.existsInTaxonomy(nid) match {
          case Success(true) => Right(nid)
          case Success(false) =>
            Left(
              ImportException(nid,
                              s"Related content with node id $nid was not found in taxonomy and will not be imported."))
          case Failure(ex) =>
            Left(
              ImportException(nid, s"Failed getting taxonomy for related content with node id $nid", Some(ex))
            )
        }
    }
  }
}
