/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters

import no.ndla.articleimport.ArticleImportProperties.{nodeTypeH5P, nodeTypeLink, nodeTypeVideo}
import no.ndla.articleimport.integration.ConverterModule.{jsoupDocumentToString, stringToJsoupDocument}
import no.ndla.articleimport.integration.{ConverterModule, LanguageContent, MigrationApiClient, MigrationEmbedMeta}
import no.ndla.articleimport.model.api.ImportException
import no.ndla.articleimport.model.domain.ImportStatus
import no.ndla.articleimport.service.converters.contentbrowser.{
  H5PConverterModule,
  LenkeConverterModule,
  VideoConverterModule
}
import no.ndla.articleimport.service.{ExtractService, TagsService}
import no.ndla.network.NdlaClient
import org.jsoup.nodes.Element

import scala.util.{Failure, Success, Try}

trait LeafNodeConverter {
  this: VideoConverterModule
    with HtmlTagGenerator
    with H5PConverterModule
    with LenkeConverterModule
    with ExtractService
    with MigrationApiClient
    with TagsService
    with NdlaClient
    with TagsService =>

  object LeafNodeConverter extends ConverterModule {
    val defaultMetaDescription = "Beskrivelse mangler"

    def convert(content: LanguageContent, importStatus: ImportStatus): Try[(LanguageContent, ImportStatus)] = {

      val newContent = content.nodeType match {
        case `nodeTypeVideo` => doVideo(content, importStatus)
        case `nodeTypeH5P`   => doH5P(content, importStatus)
        case `nodeTypeLink`  => doLink(content, importStatus)
        case _               => Success(content, importStatus)
      }

      newContent.map { case (cont, updatedStatus) => (cont, updatedStatus) }
    }

    private def doLink(cont: LanguageContent, importStatus: ImportStatus): Try[(LanguageContent, ImportStatus)] = {
      extractService.getLinkEmbedMeta(cont.nid) match {
        case Success(MigrationEmbedMeta(Some(url), embedCode)) =>
          val inlineEmbed = LenkeConverterModule.insertInline(cont.nid, url, embedCode.getOrElse(""), importStatus)

          inlineEmbed.map {
            case (embedTag, requiredLibraries, status) =>
              (cont.copy(content = embedTag, requiredLibraries = requiredLibraries.toSet, metaDescription = url),
               status)
          }

        case Success(MigrationEmbedMeta(url, embedCode)) =>
          Failure(
            ImportException(cont.nid,
                            s"External embed meta is missing url or embed code (url='$url', embedCode='$embedCode')"))
        case Failure(ex) => Failure(ex)
      }
    }

    private def doVideo(cont: LanguageContent, importStatus: ImportStatus): Try[(LanguageContent, ImportStatus)] = {
      val el = stringToJsoupDocument(cont.content)
      el.prepend(s"<section>${VideoConverterModule.toInlineVideo("", cont.nid)}</section>")
      Success((cont.copy(content = jsoupDocumentToString(el), metaDescription = defaultMetaDescription), importStatus))
    }

    private def doH5P(cont: LanguageContent, importStatus: ImportStatus): Try[(LanguageContent, ImportStatus)] = {
      val el = stringToJsoupDocument(cont.content)
      H5PConverterModule
        .toH5PEmbed(cont.nid) match {
        case Success(html) =>
          el.append(s"<section>$html</section>")
          Success(
            (cont.copy(content = jsoupDocumentToString(el), metaDescription = defaultMetaDescription), importStatus))
        case Failure(ex) =>
          if (el.text().length > 0) {
            Success(cont.copy(content = jsoupDocumentToString(el), metaDescription = defaultMetaDescription),
                    importStatus.addError(ImportException(cont.nid, ex.getMessage)))
          } else {
            Failure(ex)
          }
      }
    }

  }
}
