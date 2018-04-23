/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters.contentbrowser

import com.typesafe.scalalogging.LazyLogging
import no.ndla.articleimport.model.api.{ImportException, Article, Concept}
import no.ndla.articleimport.model.domain.{ImportStatus, RequiredLibrary}
import no.ndla.articleimport.service.ExtractConvertStoreContent
import no.ndla.articleimport.service.converters.HtmlTagGenerator
import no.ndla.validation.ValidationException

import scala.util.{Failure, Success, Try}

trait BegrepConverterModule {
  this: HtmlTagGenerator with ExtractConvertStoreContent =>

  object BegrepConverter extends ContentBrowserConverterModule with LazyLogging {
    override val typeName: String = "begrep"

    override def convert(content: ContentBrowser,
                         importStatus: ImportStatus): Try[(String, Seq[RequiredLibrary], ImportStatus)] = {
      val nodeId = content.get("nid")
      extractConvertStoreContent.processNode(nodeId, importStatus) match {
        case Success((c: Concept, is)) =>
          val embedContent = HtmlTagGenerator.buildConceptEmbedContent(c.id, content.get("link_text"))
          Success((embedContent, Seq.empty, is.addMessage(s"Imported concept with id ${c.id}")))

        case Success((x: Article, _)) =>
          val msg =
            s"THIS IS A BUG: Imported begrep node with nid $nodeId but is marked as an article (id ${x.id})"
          logger.error(msg)
          Failure(ImportException(nodeId, msg))
        case Failure(x) =>
          val exceptionMessage = x match {
            case ex: ValidationException =>
              s"${ex.getMessage}: ${ex.errors.mkString(",")}"
            case ex => ex.getMessage
          }
          val msg =
            s"Failed to import begrep with node id $nodeId: $exceptionMessage"
          logger.error(msg)
          Failure(ImportException(nodeId, msg))
      }

    }

  }
}
