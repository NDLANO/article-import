/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters.contentbrowser
import com.typesafe.scalalogging.LazyLogging
import no.ndla.articleimport.model.api.ImportException
import no.ndla.articleimport.model.domain.{ContentFilMeta, ImportStatus, RequiredLibrary, UploadedFile}
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
          uploadFile(nodeId, fileMeta).map(generateHtml(_, importStatus))
        case Success(fileMetas) if fileMetas.length > 1 =>
          val uploadedFiles = fileMetas.map(fileMeta => uploadFile(nodeId, fileMeta))
          val failures = uploadedFiles.collect { case Failure(ex) => ex }

          if (failures.nonEmpty) {
            Failure(failures.head)
          } else {
            val files = uploadedFiles.collect { case Success(f) => f }.toList

            val linkText = content.getOpt("link_text")

            val embed = generateFileEmbed(files, linkText, importStatus)

            Success(embed)
          }
        case Failure(e) => Failure(e)
      }
    }

    private def generateHtml(uploaded: UploadedFile,
                             importStatus: ImportStatus): (String, Seq[RequiredLibrary], ImportStatus) =
      (HtmlTagGenerator.buildAnchor(s"$Domain/files/${uploaded.filePath}",
                                    uploaded.fileMeta.fileName,
                                    uploaded.fileMeta.title,
                                    openInNewTab = false),
       Seq.empty,
       importStatus)

    private def generateFileEmbed(uploadedFiles: List[UploadedFile],
                                  linkText: Option[String],
                                  importStatus: ImportStatus): (String, Seq[RequiredLibrary], ImportStatus) = {
      val embedHtml = HtmlTagGenerator.buildFileEmbed(uploadedFiles).outerHtml()
      (linkText.getOrElse("") + embedHtml, Seq.empty, importStatus)
    }

    private def uploadFile(nodeId: String, fileMeta: ContentFilMeta) = {
      attachmentStorageService.uploadFileFromUrl(nodeId, fileMeta) match {
        case Success(filePath) => Success(UploadedFile(fileMeta, filePath))
        case Failure(ex) =>
          Failure(ImportException(nodeId, s"Failed to import file with node id $nodeId: ${ex.getMessage}", Some(ex)))
      }
    }

  }
}
