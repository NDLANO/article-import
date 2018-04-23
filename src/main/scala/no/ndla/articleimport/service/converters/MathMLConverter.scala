/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters

import no.ndla.articleimport.integration.ConverterModule.{jsoupDocumentToString, stringToJsoupDocument}
import no.ndla.articleimport.integration.{ConverterModule, LanguageContent}
import no.ndla.articleimport.model.domain.ImportStatus
import no.ndla.validation.TagAttributes.XMLNsAttribute
import org.jsoup.nodes.Element

import scala.collection.JavaConverters._
import scala.util.{Success, Try}

object MathMLConverter extends ConverterModule {

  def convert(content: LanguageContent, importStatus: ImportStatus): Try[(LanguageContent, ImportStatus)] = {
    val element = stringToJsoupDocument(content.content)
    addMathMlAttributes(element)
    convertCentering(element)

    Success(content.copy(content = jsoupDocumentToString(element)), importStatus)
  }

  def addMathMlAttributes(el: Element) = {
    el.select("math")
      .asScala
      .foreach(e => e.attr(s"$XMLNsAttribute", "http://www.w3.org/1998/Math/MathML"))
  }

  private def convertCentering(el: Element) = {
    el.select("math")
      .asScala
      .foreach(math => {
        math
          .parents()
          .asScala
          .foreach({
            case p if p.tagName() == "p" =>
              if (p.attr("style")
                    .replaceAll("\\s", "")
                    .contains("text-align:center")) {
                p.attr("data-align", "center")
              }
            case _ =>
          })
      })
  }

}
