/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.articleimport.service.converters.contentbrowser

import com.typesafe.scalalogging.LazyLogging
import no.ndla.articleimport.model.domain.{ImportStatus, RequiredLibrary}
import no.ndla.articleimport.service.ExtractService
import no.ndla.articleimport.service.converters.HtmlTagGenerator
import no.ndla.articleimport.ArticleImportProperties.H5PResizerScriptUrl
import no.ndla.articleimport.integration.H5PApiClient
import no.ndla.articleimport.model.api.ImportException

import scala.util.{Failure, Success, Try}

trait H5PConverterModule {
  this: ExtractService with HtmlTagGenerator with H5PApiClient =>

  object H5PConverter extends ContentBrowserConverterModule with LazyLogging {
    override val typeName: String = "h5p_content"

    override def convert(content: ContentBrowser, importStatus: ImportStatus): Try[(String, Seq[RequiredLibrary], ImportStatus)] = {
      val nodeId = content.get("nid")

      logger.info(s"Converting h5p_content with nid $nodeId")
      toH5PEmbed(nodeId) match {
        case Success(replacement) =>
          Success((replacement, Seq.empty, importStatus))
        case Failure(ex) =>
          Failure(ex)
      }
    }

    def toH5PEmbed(nodeId: String): Try[String] = {
      h5pApiClient.getViewFromOldId(nodeId) match {
        case Some(url) =>
          val replacement = HtmlTagGenerator.buildExternalInlineEmbedContent(url)
          Success(replacement)
        case None => Failure(ImportException(message = s"Failed to import H5P with id $nodeId: Not yet exported to new H5P service"))
      }
    }
  }
}
