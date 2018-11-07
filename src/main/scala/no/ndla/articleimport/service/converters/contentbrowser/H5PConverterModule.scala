/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters.contentbrowser

import com.typesafe.scalalogging.LazyLogging
import no.ndla.articleimport.model.domain.{ImportStatus, RequiredLibrary}
import no.ndla.articleimport.service.{ExtractConvertStoreContent, ExtractService}
import no.ndla.articleimport.service.converters.{HtmlTagGenerator, LightboxPattern}
import no.ndla.articleimport.integration.H5PApiClient
import no.ndla.articleimport.model.api.ImportException

import scala.util.{Failure, Success, Try}

trait H5PConverterModule {
  this: ExtractService with HtmlTagGenerator with H5PApiClient with ExtractConvertStoreContent =>

  object H5PConverterModule extends ContentBrowserConverterModule with LazyLogging {
    override val typeName: String = "h5p_content"

    override def convert(content: ContentBrowser,
                         importStatus: ImportStatus): Try[(String, Seq[RequiredLibrary], ImportStatus)] = {
      val nodeId = content.get("nid")
      val linkText = content.get("link_text")

      logger.info(s"Converting h5p_content with nid $nodeId")

      content.get("insertion") match {
        case "link" | LightboxPattern(_*) =>
          toH5PLink(linkText, nodeId, importStatus).map { case (link, status) => (link, Seq.empty, status) }
        case _ => toH5PEmbed(nodeId).map(replacement => (replacement, Seq.empty, importStatus))
      }
    }

    private def toH5PLink(linkText: String, nodeId: String, importStatus: ImportStatus): Try[(String, ImportStatus)] = {
      extractConvertStoreContent.processNode(nodeId, importStatus).map {
        case (content, status) =>
          (HtmlTagGenerator.buildContentLinkEmbedContent(content.id, linkText, openInNewWindow = false), status)
      }
    }

    def toH5PEmbed(nodeId: String): Try[String] = {
      h5pApiClient.getViewFromOldId(nodeId) match {
        case Some(url) =>
          val replacement =
            HtmlTagGenerator.buildExternalInlineEmbedContent(url)
          Success(replacement)
        case None =>
          Failure(ImportException(nodeId, s"Failed to import H5P with id $nodeId: Not yet exported to new H5P service"))
      }
    }

  }
}
