/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters.contentbrowser

import com.typesafe.scalalogging.LazyLogging
import no.ndla.articleimport.service.{AttachmentStorageService, ExtractService}
import no.ndla.articleimport.integration.AudioApiClient
import no.ndla.articleimport.model.api.ImportException
import no.ndla.articleimport.model.domain.{ImportStatus, RequiredLibrary}
import no.ndla.articleimport.service.converters.HtmlTagGenerator

import scala.util.{Failure, Success, Try}

trait AudioConverterModule {
  this: ExtractService with AttachmentStorageService with AudioApiClient with HtmlTagGenerator =>

  object AudioConverterModule extends ContentBrowserConverterModule with LazyLogging {
    override val typeName: String = "audio"

    override def convert(content: ContentBrowser,
                         importStatus: ImportStatus): Try[(String, Seq[RequiredLibrary], ImportStatus)] = {
      val nodeId = content.get("nid")
      val caption = content.get("link_text")
      val isInsideHtmlTable = content.DOMPath.contains("table")

      logger.info(s"Converting audio with nid $nodeId")

      toAudio(nodeId, caption, isInsideHtmlTable) match {
        case Success(audioHtml) =>
          Success((audioHtml, Seq.empty, importStatus))
        case Failure(_) =>
          Failure(ImportException(nodeId, s"Failed to import audio with node id $nodeId"))
      }
    }

    def toAudio(nodeId: String, caption: String, minimalPlayer: Boolean = false): Try[String] = {
      val playerType = if (minimalPlayer) "minimal" else "standard"
      audioApiClient
        .getOrImportAudio(nodeId)
        .map(audioId => {
          HtmlTagGenerator.buildAudioEmbedContent(audioId.toString, caption, playerType)
        })
    }

  }
}
