/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters.contentbrowser
import com.typesafe.scalalogging.LazyLogging
import no.ndla.articleimport.model.api.ImportException
import no.ndla.articleimport.model.domain.{ContentFilMeta, ImportStatus, RequiredLibrary}
import no.ndla.articleimport.service.converters.HtmlTagGenerator
import no.ndla.articleimport.service.{AttachmentStorageService, ExtractService}
import no.ndla.articleimport.ArticleImportProperties.Domain

import scala.util.{Failure, Success, Try}

trait FilConverterModule {
  this: ExtractService with AttachmentStorageService with HtmlTagGenerator =>

  object FilConverter extends ContentBrowserConverterModule with LazyLogging {
    override val typeName: String = "fil"

    override def convert(content: ContentBrowser,
                         importStatus: ImportStatus): Try[(String, Seq[RequiredLibrary], ImportStatus)] = {
      val nodeId = content.get("nid")

      extractService.getNodeFilMeta(nodeId) match {
        case Success(Seq(fileMeta)) =>
          uploadFile(nodeId, fileMeta).map(generateHtml(fileMeta, _, importStatus))
        case Success(f) if f.length != 1 =>
          Failure(
            ImportException(
              nodeId,
              s"File node contains more than one file (${f.length}). Only a link to a single file is supported"))
        case Failure(e) => Failure(e)
      }
    }

    private def generateHtml(fileMeta: ContentFilMeta,
                             filePath: String,
                             importStatus: ImportStatus): (String, Seq[RequiredLibrary], ImportStatus) =
      (HtmlTagGenerator.buildAnchor(s"$Domain/files/$filePath",
                                    fileMeta.fileName,
                                    fileMeta.fileName,
                                    openInNewTab = false),
       Seq.empty,
       importStatus)

    private def uploadFile(nodeId: String, fileMeta: ContentFilMeta) = {
      attachmentStorageService.uploadFileFromUrl(nodeId, fileMeta) match {
        case Success(filePath) => Success(filePath)
        case Failure(ex) =>
          Failure(ImportException(nodeId, s"Failed to import file with node id $nodeId: ${ex.getMessage}", Some(ex)))
      }
    }

  }
}
