/*
 * Part of NDLA article_import.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters

import com.typesafe.scalalogging.LazyLogging
import no.ndla.articleimport.integration.{ConverterModule, LanguageContent}
import no.ndla.articleimport.model.domain.ImportStatus
import no.ndla.validation.{ResourceType, TagAttributes}
import org.jsoup.nodes.Element

import scala.collection.JavaConverters._
import scala.util.{Success, Try}

  trait FileDivConverter {
    this: HtmlTagGenerator =>
    object FileDivConverter extends ConverterModule with LazyLogging {
      override def convert(content: LanguageContent, importStatus: ImportStatus): Try[(LanguageContent, ImportStatus)] = {
        val element = ConverterModule.stringToJsoupDocument(content.content)
        moveFilesAfterText(element)
        Success((content.copy(content = ConverterModule.jsoupDocumentToString(element)), importStatus))
      }

      private def moveFilesAfterText(element: Element): Unit = {
        element
          .select(s"""span[${TagAttributes.DataType}="${ResourceType.File}"]""")
          .asScala
          .foreach(d => {
            val parent = d.parent()
            val n = d.clone()
            parent.after(n)
            d.remove()
            n.tagName("div")
          })
      }

    }
}
