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

import scala.util.{Failure, Try}

trait NonExistentNodeConverterModule {

  object NonExistentNodeConverter extends ContentBrowserConverterModule with LazyLogging {
    override val typeName: String = "NodeDoesNotExist"

    override def convert(content: ContentBrowser,
                         importStatus: ImportStatus): Try[(String, Seq[RequiredLibrary], ImportStatus)] = {
      val nodeId = content.get("nid")
      Failure(ImportException(nodeId, s"Found nonexistant node with id $nodeId"))
    }
  }
}
