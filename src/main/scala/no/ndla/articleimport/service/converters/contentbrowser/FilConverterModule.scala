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
import no.ndla.articleimport.service.converters.HtmlTagGenerator
import no.ndla.articleimport.service.{AttachmentStorageService, ExtractService}
import no.ndla.articleimport.ArticleImportProperties.Domain

import scala.util.{Failure, Success, Try}

trait FilConverterModule {
  this: ExtractService with AttachmentStorageService with HtmlTagGenerator =>

  object FilConverter extends ContentBrowserConverterModule with LazyLogging {
    override val typeName: String = "fil"

    override def convert(content: ContentBrowser, importStatus: ImportStatus): Try[(String, Seq[RequiredLibrary], ImportStatus)] = {
      val nodeId = content.get("nid")
      val importedFile = for {
        fileMeta <- extractService.getNodeFilMeta(nodeId)
        filePath <- attachmentStorageService.uploadFileFromUrl(nodeId, fileMeta)
      } yield (HtmlTagGenerator.buildAnchor(s"$Domain/files/$filePath", fileMeta.fileName, fileMeta.fileName, openInNewTab=false), Seq.empty, importStatus)

      importedFile match {
        case Success(x) => Success(x)
        case Failure(x) => Failure(ImportException(s"Failed to import file with node id $nodeId: ${x.getMessage}"))
      }
    }

  }
}
