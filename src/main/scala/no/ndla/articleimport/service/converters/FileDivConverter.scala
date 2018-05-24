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

import scala.annotation.tailrec
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

    /**
      * Locates file-spans and moves them out of inline parents and converts them to divs
      * @param element Entire html element
      */
    private def moveFilesAfterText(element: Element): Unit = {
      element
        .select(s"""span[${TagAttributes.DataType}="${ResourceType.File}"]""")
        .asScala
        .foreach(d => {
          outOfInlines(d).tagName("div")
        })
    }

    /**
      * Moves element out of parent until it is in a block element
      * @param element Element to be moved out of parent
      * @return Element that is moved out of parent
      */
    @tailrec
    private def outOfInlines(element: Element): Element = {
      val parent = element.parent()
      val n = element.clone()
      parent.after(n)
      element.remove()

      if (!n.parent().isBlock) {
        outOfInlines(n)
      } else {
        n
      }
    }

  }
}
