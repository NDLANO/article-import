/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters.contentbrowser

import com.typesafe.scalalogging.LazyLogging
import no.ndla.articleimport.model.api.ImportException
import no.ndla.articleimport.model.domain.{FootNoteItem, ImportStatus, RequiredLibrary}
import no.ndla.articleimport.service.converters.HtmlTagGenerator
import no.ndla.articleimport.service.{ExtractConvertStoreContent, ExtractService}

import scala.util.{Failure, Success, Try}

trait BiblioConverterModule {
  this: ExtractService with ExtractConvertStoreContent with HtmlTagGenerator =>

  object BiblioConverter extends ContentBrowserConverterModule with LazyLogging {
    override val typeName: String = "biblio"

    override def convert(content: ContentBrowser, importStatus: ImportStatus): Try[(String, Seq[RequiredLibrary], ImportStatus)] = {
      val nodeId = content.get("nid")
      getFootNoteData(nodeId) match {
        case None => Failure(ImportException(nodeId, s"Failed to fetch biblio meta data with node id $nodeId"))
        case Some(meta) =>
          Success(HtmlTagGenerator.buildFootNoteItem(
          title = meta.title,
          `type` = meta.`type`,
          year = meta.year,
          edition = meta.edition,
          publisher = meta.publisher,
          authors = meta.authors.toSet
        ), List[RequiredLibrary](), importStatus)
      }
    }

    private def getFootNoteData(nodeId: String): Option[FootNoteItem] =
      extractService.getBiblioMeta(nodeId).map(biblioMeta => FootNoteItem(biblioMeta.biblio, biblioMeta.authors))

  }
}
