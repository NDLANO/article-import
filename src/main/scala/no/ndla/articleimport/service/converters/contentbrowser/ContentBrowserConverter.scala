/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters.contentbrowser

import com.typesafe.scalalogging.LazyLogging
import org.jsoup.nodes.Element

import scala.annotation.tailrec
import no.ndla.articleimport.integration.{ConverterModule, LanguageContent}
import no.ndla.articleimport.model.domain.ImportStatus
import no.ndla.articleimport.integration.ConverterModule.{jsoupDocumentToString, stringToJsoupDocument}
import no.ndla.articleimport.model.api.{ImportException, ImportExceptions}

import scala.util.{Failure, Success, Try}

trait ContentBrowserConverter {
  this: ContentBrowserConverterModules =>
  val contentBrowserConverter: ContentBrowserConverter

  class ContentBrowserConverter extends ConverterModule with LazyLogging {
    private val contentBrowserModules =
      Map[String, ContentBrowserConverterModule](
        ImageConverterModule.typeName -> ImageConverterModule,
        H5PConverterModule.typeName -> H5PConverterModule,
        LenkeConverterModule.typeName -> LenkeConverterModule,
        OppgaveConverterModule.typeName -> OppgaveConverterModule,
        FagstoffConverterModule.typeName -> FagstoffConverterModule,
        AktualitetConverterModule.typeName -> AktualitetConverterModule,
        NonExistentNodeConverterModule.typeName -> NonExistentNodeConverterModule,
        VideoConverterModule.typeName -> VideoConverterModule,
        VeiledningConverterModule.typeName -> VeiledningConverterModule,
        AudioConverterModule.typeName -> AudioConverterModule,
        FilConverterModule.typeName -> FilConverterModule,
        BiblioConverterModule.typeName -> BiblioConverterModule,
        BegrepConverterModule.typeName -> BegrepConverterModule
      )

    private def getConverterModule(contentBrowser: ContentBrowser) = {
      val nodeType = extractService
        .getNodeType(contentBrowser.get("nid"))
        .getOrElse(NonExistentNodeConverterModule.typeName)
      contentBrowserModules.getOrElse(nodeType, UnsupportedContentConverterModule)
    }

    def replaceHtmlInElement(element: Element, start: Int, end: Int, replacement: String) = {
      val html = element.html()
      element.html(html.substring(0, start) + replacement + html.substring(end))
    }

    def convert(languageContent: LanguageContent, importStatus: ImportStatus): Try[(LanguageContent, ImportStatus)] = {
      @tailrec def convert(element: Element,
                           languageContent: LanguageContent,
                           importStatus: ImportStatus): (LanguageContent, ImportStatus) = {
        val cont =
          ContentBrowserString(element, languageContent.language)

        if (!cont.IsContentBrowserField)
          return (languageContent, importStatus)
        getConverterModule(cont).convert(cont, importStatus) match {
          case Failure(x) =>
            val (start, end) = cont.StartEndIndex
            replaceHtmlInElement(element, start, end, HtmlTagGenerator.buildErrorContent("Innhold mangler."))
            val ex = ImportException(cont.getOpt("nid").getOrElse(languageContent.nid),
                                     "ContentBrowserConverter failed",
                                     Some(x))
            convert(element, languageContent, importStatus.addError(ex))
          case Success((newContent, reqLibs, status)) =>
            val (start, end) = cont.StartEndIndex
            replaceHtmlInElement(element, start, end, newContent)

            val updatedRequiredLibraries = languageContent.requiredLibraries ++ reqLibs
            convert(element, languageContent.copy(requiredLibraries = updatedRequiredLibraries), status)
        }
      }

      val contentElement = stringToJsoupDocument(languageContent.content)
      val (updatedLanguageContent, updatedImportStatus) =
        convert(contentElement, languageContent, importStatus)

      val metaDescriptionElement = stringToJsoupDocument(languageContent.metaDescription)
      val (finalLanguageContent, finalImportStatus) =
        convert(metaDescriptionElement, updatedLanguageContent, updatedImportStatus)

      Success(
        (finalLanguageContent.copy(content = jsoupDocumentToString(contentElement),
                                   metaDescription = jsoupDocumentToString(metaDescriptionElement)),
         finalImportStatus))
    }
  }
}
