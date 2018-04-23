/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters.contentbrowser

import com.typesafe.scalalogging.LazyLogging
import no.ndla.articleimport.model.api.ImportException
import no.ndla.articleimport.model.domain.{ImportStatus, RequiredLibrary}
import no.ndla.articleimport.service.ExtractService
import no.ndla.articleimport.service.converters.HtmlTagGenerator

import scala.util.{Failure, Try}

trait UnsupportedContentConverter {
  this: ExtractService with HtmlTagGenerator =>

  object UnsupportedContentConverter extends ContentBrowserConverterModule with LazyLogging {
    override val typeName: String = "unsupported content"

    override def convert(content: ContentBrowser,
                         importStatus: ImportStatus): Try[(String, Seq[RequiredLibrary], ImportStatus)] = {
      val nodeId = content.get("nid")
      val nodeType =
        extractService.getNodeType(content.get("nid")).getOrElse("unknown")
      val errorMessage =
        s"Unsupported content $nodeType in node with id $nodeId"
      logger.error(errorMessage)
      Failure(ImportException(nodeId, errorMessage))
    }
  }

}
