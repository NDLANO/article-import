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
        case `nodeTypeVideo` => doVideo(content)
        case `nodeTypeH5P`   => doH5P(content)
        case `nodeTypeLink`  => doLink(content)
        case _               => Success(content)
      }

      newContent.map(cont => (cont, importStatus))
    }

    private def doLink(cont: LanguageContent): Try[LanguageContent] = {
      extractService.getLinkEmbedMeta(cont.nid) match {
        case Success(MigrationEmbedMeta(Some(url), embedCode)) =>
          val inlineEmbed = LenkeConverterModule.insertInline(cont.nid, url, embedCode.getOrElse(""))

          inlineEmbed.map {
            case (embedTag, requiredLibraries, _) =>
              cont.copy(content = embedTag, requiredLibraries = requiredLibraries.toSet, metaDescription = url)
          }

        case Success(MigrationEmbedMeta(url, embedCode)) =>
          Failure(
            ImportException(cont.nid,
                            s"External embed meta is missing url or embed code (url='$url', embedCode='$embedCode')"))
        case Failure(ex) => Failure(ex)
      }
    }

    private def doVideo(cont: LanguageContent): Try[LanguageContent] = {
      val el = stringToJsoupDocument(cont.content)
      el.prepend(s"<section>${VideoConverterModule.toInlineVideo("", cont.nid)}</section>")
      Success(cont.copy(content = jsoupDocumentToString(el), metaDescription = defaultMetaDescription))
    }

    private def doH5P(cont: LanguageContent): Try[LanguageContent] = {
      val el = stringToJsoupDocument(cont.content)
      H5PConverterModule
        .toH5PEmbed(cont.nid)
        .map(html => {
          el.append(s"<section>$html</section>")
          cont.copy(content = jsoupDocumentToString(el), metaDescription = defaultMetaDescription)
        })
    }

  }
}
