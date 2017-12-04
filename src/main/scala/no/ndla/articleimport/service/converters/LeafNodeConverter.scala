/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.articleimport.service.converters

import no.ndla.articleimport.ArticleImportProperties.{nodeTypeH5P, nodeTypeVideo}
import no.ndla.articleimport.integration.{ConverterModule, LanguageContent, MigrationApiClient}
import no.ndla.articleimport.model.domain.ImportStatus
import no.ndla.articleimport.service.{ExtractService, TagsService}
import no.ndla.articleimport.service.converters.contentbrowser.{H5PConverterModule, VideoConverterModule}
import no.ndla.network.NdlaClient
import no.ndla.articleimport.integration.ConverterModule.{jsoupDocumentToString, stringToJsoupDocument}

import scala.util.{Failure, Success, Try}

trait LeafNodeConverter {
  this: VideoConverterModule with HtmlTagGenerator with H5PConverterModule with ExtractService with MigrationApiClient with TagsService with NdlaClient with TagsService =>

  object LeafNodeConverter extends ConverterModule {

    def convert(content: LanguageContent, importStatus: ImportStatus): Try[(LanguageContent, ImportStatus)] = {
      val element = stringToJsoupDocument(content.content)

      val requiredLibraries = content.nodeType match {
        case `nodeTypeVideo` =>
          val html = VideoConverter.toInlineVideo("", content.nid)
          element.prepend(s"<section>$html</section>")
          Success(content.requiredLibraries)
        case `nodeTypeH5P` =>
          H5PConverter.toH5PEmbed(content.nid) match {
            case Success(html) =>
              element.prepend(s"<section>$html</section>")
              Success(content.requiredLibraries)
            case Failure(ex) => Failure(ex)
          }
        case _ => Success(content.requiredLibraries)
      }

      requiredLibraries match {
        case Success(requiredLib) =>
          Success(content.copy(content=jsoupDocumentToString(element), requiredLibraries=requiredLib), importStatus)
        case Failure(ex) => Failure(ex)
      }
    }

  }
}
