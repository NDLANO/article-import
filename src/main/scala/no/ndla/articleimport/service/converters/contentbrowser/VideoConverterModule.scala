/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters.contentbrowser

import com.typesafe.scalalogging.LazyLogging
import no.ndla.articleimport.ArticleImportProperties.{NDLABrightcoveAccountId, NDLABrightcovePlayerId}
import no.ndla.articleimport.model.domain.{ImportStatus, RequiredLibrary}
import no.ndla.articleimport.service.ExtractConvertStoreContent
import no.ndla.articleimport.service.converters.HtmlTagGenerator

import scala.util.{Failure, Success, Try}

trait VideoConverterModule {
  this: HtmlTagGenerator with ExtractConvertStoreContent =>

  object VideoConverterModule extends ContentBrowserConverterModule with LazyLogging {
    override val typeName: String = "video"

    override def convert(content: ContentBrowser,
                         importStatus: ImportStatus): Try[(String, Seq[RequiredLibrary], ImportStatus)] = {
      val (linkText, nodeId) = (content.get("link_text"), content.get("nid"))
      val LightboxPattern = "(lightbox_.*)".r

      val video = content.get("insertion") match {
        case "link" | LightboxPattern(_) => toVideoLink(linkText, nodeId, importStatus)
        case _                           => Success((toInlineVideo(linkText, nodeId), importStatus))
      }

      video.map {
        case (embedVideo, updatedStatus) =>
          logger.info(s"Added video with nid ${content.get("nid")}")
          (embedVideo, Seq.empty, updatedStatus)
      }
    }

    private def toVideoLink(linkText: String,
                            nodeId: String,
                            importStatus: ImportStatus): Try[(String, ImportStatus)] = {
      extractConvertStoreContent.processNode(nodeId, importStatus) match {
        case Success((content, status)) =>
          Success(HtmlTagGenerator.buildContentLinkEmbedContent(content.id, linkText, openInNewWindow = false), status)
        case Failure(ex) => Failure(ex)
      }
    }

    def toInlineVideo(linkText: String, nodeId: String): String = {
      HtmlTagGenerator.buildBrightCoveEmbedContent(caption = linkText,
                                                   videoId = s"ref:$nodeId",
                                                   account = s"$NDLABrightcoveAccountId",
                                                   player = s"$NDLABrightcovePlayerId")
    }

  }
}
