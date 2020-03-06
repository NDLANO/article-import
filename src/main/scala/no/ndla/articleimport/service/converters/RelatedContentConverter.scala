/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters

import cats.data.OptionT
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import no.ndla.articleimport.ArticleImportProperties.{importRelatedNodesMaxDepth, nodeTypeLink, supportedContentTypes}
import no.ndla.articleimport.integration.ConverterModule.{jsoupDocumentToString, stringToJsoupDocument}
import no.ndla.articleimport.integration._
import no.ndla.articleimport.model.api.{Article, Concept, ImportException, ImportExceptions}
import no.ndla.articleimport.model.domain.{ExternalEmbedMetaWithTitle, ImportStatus, Language}
import no.ndla.articleimport.service.{ExtractConvertStoreContent, ExtractService}

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

      val allRelatedNids =
        content.relatedContent.map(c => (c.nid, extractService.getNodeType(c.nid).getOrElse("unknown"))).toSet
      val linkNodesWithOnlyUrl = getLinkNodesWithoutEmbed(allRelatedNids, content.language)
      val nonLinkNids = allRelatedNids.filterNot { case (nid, _) => linkNodesWithOnlyUrl.map(_.nid).contains(nid) }
      val (excludedNids, nidsToImport) = getValidNids(nonLinkNids.toList).separate

      val updatedStatus = importStatus.addErrors(excludedNids)

      handleNodes(content.nid, nidsToImport.toSet, updatedStatus) match {
        case Success((ids, status)) if ids.nonEmpty || linkNodesWithOnlyUrl.nonEmpty =>
          val element = stringToJsoupDocument(content.content)
          element.append(
            s"<section>${HtmlTagGenerator.buildRelatedContent(ids.toList, linkNodesWithOnlyUrl.toList)}</section>")
          Success(content.copy(content = jsoupDocumentToString(element)), status)
        case Success((_, status)) =>
          Success(content, status)
        case Failure(ex) =>
          Success((content, handleRelatedErrors(content.nid, ex, updatedStatus)))
      }
    }

    private def handleRelatedErrors(nid: String, exception: Throwable, importStatus: ImportStatus): ImportStatus = {
      exception match {
        case ex: ImportExceptions =>
          val importExceptions = ex.errors.collect { case ex: ImportException => ex }
          val otherExceptions = ex.errors.diff(importExceptions)
          importStatus
            .addErrors(importExceptions)
            .addErrors(otherExceptions.map(e =>
              ImportException(nid, "Something unexpected went wrong while importing related nodes.", Some(e))))
        case ex =>
          importStatus.addError(ImportException(nid, ex.getMessage, Some(ex)))
      }

    }

    /**
      * @param nidsWithType Set of tuples with (NodeId, NodeType)
      * @param language Language in ISO639 format
      * @return Returns set of nid, title and url for linknodes without embed codes listed in nidsWithType
      */
    private def getLinkNodesWithoutEmbed(nidsWithType: Set[(String, String)],
                                         language: String): Set[ExternalEmbedMetaWithTitle] = {
      nidsWithType.collect {
        case (nid, `nodeTypeLink`) =>
          extractService.getLinkEmbedMeta(nid) match {
            case Success(MigrationEmbedMeta(Some(url), None)) =>
              extractService.getNodeData(nid).toOption.map { node =>
                val linkTitle =
                  Language.findByLanguageOrBestEffort(node.titles, language).map(_.title).getOrElse("")
                ExternalEmbedMetaWithTitle(nid, linkTitle, url)
              }
            case _ => None
          }
      }.flatten
    }

    /**
      * Imports or fetches nodes depending on whether the node depth threshold is bigger than [[importRelatedNodesMaxDepth]]
      * @param nid mainNodeId
      * @param nidsToImport nids that are to be imported or fetched.
      * @param importStatus ImportStatus
      * @return Try with a tuple where _1 is set of imported/fetched ids and _2 is updated importStatus
      */
    private def handleNodes(nid: String,
                            nidsToImport: Set[String],
                            importStatus: ImportStatus): Try[(Set[Long], ImportStatus)] = {
      if (importStatus.nodeLocalContext.depth > importRelatedNodesMaxDepth) {
        getRelatedContentFromDb(nidsToImport, importStatus)
      } else {
        importRelatedContent(nid, nidsToImport, importStatus)
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

    private def getRelatedContentFromDb(nids: Set[String],
                                        importStatus: ImportStatus): Try[(Set[Long], ImportStatus)] = {
      val ids = nids.flatMap(draftApiClient.getArticleIdFromExternalId)
      Success((ids, importStatus))
    }

    private def existsInTaxonomy(nid: String): Try[Boolean] = {
      taxonomyApiClient.getResource(nid).map(_.isDefined) match {
        case Success(true) => Success(true)
        case _             => taxonomyApiClient.getTopic(nid).map(_.isDefined)
      }
    }

    private def atLeastOneExistsInTaxonomy(nid: String): Try[Boolean] = {
      extractService
        .getNodeData(nid)
        .flatMap(node => {
          // Convert to stream so we only to api calls if necessary
          val taxonomyResponses = node.contents.to(LazyList).map(_.nid).map(existsInTaxonomy)

          taxonomyResponses
            .collectFirst { case Success(exists) if exists              => Success(exists) } // Finds if the node is in taxonomy
            .orElse { taxonomyResponses.collectFirst { case Failure(ex) => Failure(ex) } } // Checks whether there was any errors
            .getOrElse { Success(false) } // If not found nor any errors we return Success(false)
        })
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
          atLeastOneExistsInTaxonomy(nid) match {
            case Success(true) => Right(nid)
            case Success(false) =>
              Left(
                ImportException(
                  nid,
                  s"Related content with node id $nid was not found in taxonomy and will not be imported."))
            case Failure(ex) =>
              Left(
                ImportException(nid, s"Failed getting taxonomy for related content with node id $nid", Some(ex))
              )
          }
      }
    }
  }
}
