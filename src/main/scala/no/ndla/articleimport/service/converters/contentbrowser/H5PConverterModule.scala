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

import scala.util.{Success, Try}

trait H5PConverterModule {
  this: ExtractService with HtmlTagGenerator =>

  object H5PConverter extends ContentBrowserConverterModule with LazyLogging {
    override val typeName: String = "h5p_content"

    override def convert(content: ContentBrowser, importStatus: ImportStatus): Try[(String, Seq[RequiredLibrary], ImportStatus)] = {
      val nodeId = content.get("nid")

      logger.info(s"Converting h5p_content with nid $nodeId")
      val (replacement, requiredLibrary) = toH5PEmbed(nodeId)
      Success(replacement, Seq(requiredLibrary), importStatus)
    }

    def toH5PEmbed(nodeId: String): (String, RequiredLibrary) = {
      val requiredLibrary = RequiredLibrary("text/javascript", "H5P-Resizer", H5PResizerScriptUrl)
      val replacement = HtmlTagGenerator.buildH5PEmbedContent(s"//ndla.no/h5p/embed/$nodeId")
      (replacement, requiredLibrary)
    }
  }
}
