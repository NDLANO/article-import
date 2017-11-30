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
import no.ndla.articleimport.ArticleImportProperties.EnableJoubelH5POembed
import no.ndla.articleimport.integration.ConverterModule.{jsoupDocumentToString, stringToJsoupDocument}
import no.ndla.articleimport.model.api.ImportException

import scala.util.{Failure, Success, Try}

trait ContentBrowserConverter {
  this: ContentBrowserConverterModules =>
  val contentBrowserConverter: ContentBrowserConverter

  class ContentBrowserConverter extends ConverterModule with LazyLogging {
    private val contentBrowserModules = Map[String, ContentBrowserConverterModule](
      ImageConverter.typeName -> ImageConverter,
      if (EnableJoubelH5POembed) JoubelH5PConverter.typeName -> JoubelH5PConverter else H5PConverter.typeName -> H5PConverter,
      LenkeConverter.typeName -> LenkeConverter,
      OppgaveConverter.typeName -> OppgaveConverter,
      FagstoffConverter.typeName -> FagstoffConverter,
      AktualitetConverter.typeName -> AktualitetConverter,
      NonExistentNodeConverter.typeName -> NonExistentNodeConverter,
      VideoConverter.typeName -> VideoConverter,
      VeiledningConverter.typeName -> VeiledningConverter,
      AudioConverter.typeName -> AudioConverter,
      FilConverter.typeName -> FilConverter,
      BiblioConverter.typeName -> BiblioConverter,
      BegrepConverter.typeName -> BegrepConverter)

    private def getConverterModule(contentBrowser: ContentBrowser) = {
      val nodeType = extractService.getNodeType(contentBrowser.get("nid")).getOrElse(NonExistentNodeConverter.typeName)
      contentBrowserModules.getOrElse(nodeType, UnsupportedContentConverter)
    }

    def replaceHtmlInElement(element: Element, start: Int, end: Int, replacement: String) = {
      val html = element.html()
      element.html(html.substring(0, start) + replacement + html.substring(end))
    }

    def convert(languageContent: LanguageContent, importStatus: ImportStatus): Try[(LanguageContent, ImportStatus)] = {
      @tailrec def convert(element: Element, languageContent: LanguageContent, importStatus: ImportStatus, exceptions: Seq[Throwable]): (LanguageContent, ImportStatus, Seq[Throwable]) = {
        val cont = ContentBrowser(element.html(), languageContent.language)

        if (!cont.IsContentBrowserField)
          return (languageContent, importStatus, exceptions)

        val converterModule = getConverterModule(cont)

        converterModule.convert(cont, importStatus) match {
          case Failure(x) =>
            val (start, end) = cont.StartEndIndex
            replaceHtmlInElement(element, start, end, "")
            convert(element, languageContent, importStatus, exceptions :+ x)
          case Success((newContent, reqLibs, status)) =>
            val (start, end) = cont.StartEndIndex
            replaceHtmlInElement(element, start, end, newContent)

            val updatedRequiredLibraries = languageContent.requiredLibraries ++ reqLibs
            convert(element, languageContent.copy(requiredLibraries = updatedRequiredLibraries), status, exceptions)
        }
      }

      val contentElement = stringToJsoupDocument(languageContent.content)
      val (updatedLanguageContent, updatedImportStatus, contentExceptions) = convert(contentElement, languageContent, importStatus, Seq())

      val metaDescriptionElement = stringToJsoupDocument(languageContent.metaDescription)
      val (finalLanguageContent, finalImportStatus, migrationContentExceptions) = convert(metaDescriptionElement, updatedLanguageContent, updatedImportStatus, Seq())

      val converterExceptions = contentExceptions ++ migrationContentExceptions
      converterExceptions.headOption match {
        case Some(_) =>
          val exceptionMessages = converterExceptions.map(_.getMessage)
          Failure(ImportException(s"Error(s) in ContentBrowserConverter: ${exceptionMessages.mkString(",")}"))
        case None =>
          Success(finalLanguageContent.copy(content=jsoupDocumentToString(contentElement), metaDescription=jsoupDocumentToString(metaDescriptionElement)),
            finalImportStatus)
      }
    }

  }
}