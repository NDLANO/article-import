/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters.contentbrowser

import com.typesafe.scalalogging.LazyLogging
import no.ndla.articleimport.integration.DraftApiClient
import no.ndla.articleimport.model.api.{Article, Concept, ImportException}
import no.ndla.articleimport.model.domain.{ImportStatus, Language, NodeToConvert, RequiredLibrary}
import no.ndla.articleimport.service.converters.{HtmlTagGenerator, LightboxPattern}
import no.ndla.articleimport.service.{ExtractConvertStoreContent, ExtractService}
import no.ndla.articleimport.ArticleImportProperties.supportedTextTypes

import scala.util.{Failure, Success, Try}

trait GeneralContentConverterModule {
  this: ExtractService with ExtractConvertStoreContent with HtmlTagGenerator with DraftApiClient =>

  abstract class GeneralContentConverterModule extends ContentBrowserConverterModule with LazyLogging {
    override def convert(contentBrowser: ContentBrowser,
                         importStatus: ImportStatus): Try[(String, Seq[RequiredLibrary], ImportStatus)] = {
      val externalId = contentBrowser.get("nid")
      val contents =
        extractService.getNodeGeneralContent(externalId).sortBy(c => c.language)
      val contentNodeData = extractService.getNodeData(externalId)

      contents.reverse.find(c => c.language == contentBrowser.language | c.language == Language.NoLanguage) match {
        case Some(content) =>
          insertContent(content.content, contentNodeData, contentBrowser, importStatus).map {
            case (finalContent, status) => (finalContent, Seq.empty, status)
          }
        case None =>
          Failure(
            ImportException(externalId,
                            s"Failed to retrieve '$typeName' with language '${contentBrowser.language}' ($externalId)"))
      }
    }

    private def mergeLicenseAndAuthors(contentNodeData: Try[NodeToConvert], importStatus: ImportStatus) = {
      val licenseToInsert = contentNodeData.map(_.license).getOrElse(None)
      val authorsToInsert = contentNodeData.map(_.authors).getOrElse(List.empty).toList
      val combineLicenses = contentNodeData.map(n => supportedTextTypes.contains(n.nodeType)).getOrElse(false)
      if (combineLicenses) importStatus.addInsertedAuthors(authorsToInsert).addInsertedLicense(licenseToInsert)
      else importStatus
    }

    def insertContent(content: String,
                      contentNodeData: Try[NodeToConvert],
                      contentBrowser: ContentBrowser,
                      importStatus: ImportStatus): Try[(String, ImportStatus)] = {
      val insertionMethod = contentBrowser.get("insertion")

      insertionMethod match {
        case "inline" => Success(content, mergeLicenseAndAuthors(contentNodeData, importStatus))
        case "collapsed_body" =>
          Success(HtmlTagGenerator.buildDetailsSummaryContent(contentBrowser.get("link_text"), content),
                  mergeLicenseAndAuthors(contentNodeData, importStatus))
        case "link" =>
          insertLink(contentBrowser, importStatus, openInNewWindow = false)
        case LightboxPattern(_) =>
          insertLink(contentBrowser, importStatus, openInNewWindow = true)
        case _ =>
          val warnMessage =
            s"""Unhandled insertion method '$insertionMethod' on '${contentBrowser
              .get("link_text")}'. Defaulting to link."""
          logger.warn(warnMessage)
          insertLink(contentBrowser, importStatus, openInNewWindow = false).map {
            case (insertString, is) =>
              (insertString, is.addMessage(warnMessage))
          }
      }
    }

    def insertLink(contentBrowser: ContentBrowser,
                   importStatus: ImportStatus,
                   openInNewWindow: Boolean): Try[(String, ImportStatus)] = {
      val externalId = contentBrowser.get("nid")

      getContentId(externalId, importStatus) match {
        case Success((Some(articleId), _, is)) =>
          val embedContent =
            HtmlTagGenerator.buildContentLinkEmbedContent(articleId, contentBrowser.get("link_text"), openInNewWindow)
          Success(s" $embedContent", is)
        case Success((_, Some(conceptId), is)) =>
          val embedContent = HtmlTagGenerator.buildConceptEmbedContent(conceptId, contentBrowser.get("link_text"))
          Success(s" $embedContent", is)
        case Success((None, None, _)) =>
          Failure(ImportException(externalId, s"Failed to retrieve or import article with external id $externalId"))
        case Failure(e) => Failure(e)
      }
    }

    private def getContentId(externalId: String,
                             importStatus: ImportStatus): Try[(Option[Long], Option[Long], ImportStatus)] = {
      val mainNodeId = extractConvertStoreContent
        .getMainNodeId(externalId)
        .getOrElse(externalId)

      (draftApiClient.getArticleIdFromExternalId(mainNodeId), draftApiClient.getConceptIdFromExternalId(mainNodeId)) match {
        case (None, None) =>
          logger.info(s"Article with node id $mainNodeId does not exist. Importing it!")
          extractConvertStoreContent.processNode(mainNodeId, importStatus) match {
            case Success((c: Article, is)) => Success(Some(c.id), None, is)
            case Success((c: Concept, is)) => Success(None, Some(c.id), is)
            case Failure(ex)               => Failure(ex)
          }
        case (Some(articleId), _) =>
          Success(Some(articleId), None, importStatus)
        case (None, Some(conceptid)) =>
          Success(None, Some(conceptid), importStatus)
      }
    }

  }
}
