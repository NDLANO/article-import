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
import org.jsoup.nodes.Element

import scala.jdk.CollectionConverters._
import scala.util.{Success, Try}

object TableConverter extends ConverterModule {
  override def convert(content: LanguageContent, importStatus: ImportStatus): Try[(LanguageContent, ImportStatus)] = {
    val element = stringToJsoupDocument(content.content)

    stripParagraphTag(element)
    convertFirstTrToTh(element)
    wrapHeaderRowInThead(element)

    Success(content.copy(content = jsoupDocumentToString(element)), importStatus)
  }

  def wrapHeaderRowInThead(el: Element): Unit = {
    for (table <- el.select("table").asScala) {
      if (table.select("thead").asScala.isEmpty) {
        table.select("tr>th").parents.first() match {
          // Only wrap row in thead if all cells are th
          case row: Element if row.select("th").size() == row.childNodeSize() =>
            table.prepend(row.outerHtml())
            row.remove()
            table.select("tbody").first.tagName("thead")
          case _ =>
        }
        table
          .select("tbody")
          .asScala
          .foreach(tb => if (tb.childNodeSize() == 0) tb.remove())
      }
    }
  }

  def stripParagraphTag(el: Element): Unit = {
    for (cell <- el.select("td").asScala) {
      val paragraphs = cell.select("p")
      if (paragraphs.size() == 1) {
        paragraphs.first.unwrap
      }
    }
  }

  def convertFirstTrToTh(el: Element): Unit = {
    for (table <- el.select("table").asScala) {
      Option(table.select("tr").first).foreach(firstRow => {
        if (containsTag(firstRow, "strong") && !containsTag(firstRow, "th")) {
          firstRow.select("td").tagName("th")
        }
        firstRow.select("strong").asScala.foreach(_.unwrap())
      })
    }
  }

  private def containsTag(el: Element, tagName: String): Boolean = el.select(tagName).asScala.nonEmpty

}
