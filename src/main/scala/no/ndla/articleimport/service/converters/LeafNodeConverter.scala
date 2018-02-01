/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.articleimport.service.converters

import no.ndla.articleimport.ArticleImportProperties.{nodeTypeH5P, nodeTypeVideo}
import no.ndla.articleimport.integration.ConverterModule.{jsoupDocumentToString, stringToJsoupDocument}
import no.ndla.articleimport.integration.{ConverterModule, LanguageContent, MigrationApiClient}
import no.ndla.articleimport.model.domain.ImportStatus
import no.ndla.articleimport.service.converters.contentbrowser.{H5PConverterModule, VideoConverterModule}
import no.ndla.articleimport.service.{ExtractService, TagsService}
import no.ndla.network.NdlaClient
import org.jsoup.nodes.Element

import scala.util.{Success, Try}

trait LeafNodeConverter {
  this: VideoConverterModule with HtmlTagGenerator with H5PConverterModule with ExtractService with MigrationApiClient with TagsService with NdlaClient with TagsService =>

  object LeafNodeConverter extends ConverterModule {

    def convert(content: LanguageContent, importStatus: ImportStatus): Try[(LanguageContent, ImportStatus)] = {
      val element = stringToJsoupDocument(content.content)
      val defaltMetaDescription = "standard metatekst"

      val newContent = content.nodeType match {
        case `nodeTypeVideo` => doVideo(element, content.nid)
        case `nodeTypeH5P` => doH5P(element, content.nid)
        case _ => Success(content.content)
      }

      newContent.map(c =>
        (content.copy(content=c, metaDescription = defaltMetaDescription), importStatus)
      )
    }

    private def doVideo(el: Element, nid: String): Try[String] = {
      el.prepend(s"<section>${VideoConverter.toInlineVideo("", nid)}</section>")
      Success(jsoupDocumentToString(el))
    }

    private def doH5P(el: Element, nid: String): Try[String] = {
      H5PConverter.toH5PEmbed(nid).map(html => {
        el.append(s"<section>$html</section>")
        jsoupDocumentToString(el)
      })
    }

  }
}
