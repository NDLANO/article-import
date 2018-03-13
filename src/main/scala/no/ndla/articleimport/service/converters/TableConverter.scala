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

import scala.collection.JavaConverters._
import scala.util.{Success, Try}

object TableConverter extends ConverterModule {
  override def convert(content: LanguageContent, importStatus: ImportStatus): Try[(LanguageContent, ImportStatus)] = {
    val element = stringToJsoupDocument(content.content)

    stripParagraphTag(element)
    convertFirstTrToTh(element)
    wrapHeaderRowInThead(element)
    unwrapStrongsInTHead(element)

    Success(content.copy(content=jsoupDocumentToString(element)), importStatus)
  }

  private def wrapHeaderRowInThead(el: Element): Unit = {
    for (table <- el.select("table").asScala) {
      if (table.select("thead").asScala.isEmpty) {
        table.select("tr>th").parents.first() match {
          case row: Element if row.select("td").isEmpty =>
            table.prependChild(row)
            row.wrap("<thead></thead>")
          case _ =>
        }
        table.select("tbody").asScala.foreach(tb => if (tb.childNodeSize() == 0) tb.remove())
      }
    }
  }

  private def stripParagraphTag(el: Element): Unit = {
    for (cell <- el.select("td").asScala) {
      val paragraphs = cell.select("p")
      if (paragraphs.size() == 1) {
        paragraphs.first.unwrap
      }
    }
  }

  private def unwrapStrongsInTHead(el: Element): Unit = {
    el.select("table>thead>tr>th").asScala.foreach(th => th.select("strong").unwrap() )
  }

  private def convertFirstTrToTh(el: Element): Unit = {
    for (table <- el.select("table").asScala) {
      Option(table.select("tr").first).foreach(firstRow => {
        val alreadyContainsThTag = firstRow.select("th").asScala.nonEmpty
        if (!alreadyContainsThTag && firstRow.select("strong").asScala.nonEmpty) {
          firstRow.select("td").tagName("th")
        }
      })
    }
  }

}
