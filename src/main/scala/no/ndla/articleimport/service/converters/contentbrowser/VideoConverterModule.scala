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

      val (embedVideo, updatedStatus) = content.get("insertion") match {
        case "link" =>
          toVideoLink(linkText, nodeId, importStatus) match {
            case Success(link) => link
            case Failure(e)    => return Failure(e)
          }
        case _ => (toInlineVideo(linkText, nodeId), importStatus)
      }

      logger.info(s"Added video with nid ${content.get("nid")}")
      Success(embedVideo, Seq.empty, updatedStatus)
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
