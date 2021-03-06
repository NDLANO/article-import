/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters

import no.ndla.articleimport.integration.ConverterModule.{jsoupDocumentToString, stringToJsoupDocument}
import no.ndla.articleimport.integration.{ConverterModule, LanguageContent}
import no.ndla.articleimport.model.api.ImportException
import no.ndla.articleimport.model.domain.ImportStatus
import no.ndla.validation.{TagAttributes, TextValidator}
import org.jsoup.nodes.Element

import scala.jdk.CollectionConverters._
import scala.util.{Success, Try}

trait SimpleTagConverter {
  this: HtmlTagGenerator =>

  object SimpleTagConverter extends ConverterModule {

    def convert(content: LanguageContent, importStatus: ImportStatus): Try[(LanguageContent, ImportStatus)] = {
      val element = stringToJsoupDocument(content.content)
      convertDivs(element)
      convertPres(element)
      convertHeadings(element)
      convertSpans(element)

      handleEmbed(content, element, importStatus)
    }

    private def handleEmbed(content: LanguageContent,
                            element: Element,
                            importStatus: ImportStatus): Try[(LanguageContent, ImportStatus)] = {
      val allEmbeds = element.select("embed").asScala.toList
      val HtmlValidator = new TextValidator(allowHtml = true)

      val invalidEmbeds = allEmbeds.filterNot(e => {
        HtmlValidator.validate("content", e.outerHtml(), validateEmbedTagParent = false).isEmpty
      })

      val errorMessages = invalidEmbeds.map(embed => {
        val errorToReturn =
          s"Failed to import node with invalid embed. (${embed.outerHtml()})"

        val errorEmbed = HtmlTagGenerator.buildErrorContent("Innhold mangler.")
        embed.after(errorEmbed)
        embed.remove()
        errorToReturn
      })

      Success(
        (content.copy(content = jsoupDocumentToString(element)),
         importStatus.addErrors(errorMessages.map(err => ImportException(content.nid, err)))))
    }

    def convertDivs(el: Element): Unit = {
      for (el <- el.select("div").asScala) {
        el.className() match {
          case "right"     => replaceTag(el, "aside")
          case "paragraph" => replaceTag(el, "section")
          case "quote"     => replaceTag(el, "blockquote")
          case "hide"      => handle_hide(el)
          case "frame" =>
            el.removeClass("frame")
            el.addClass("c-bodybox")
          case "full" | "wrapicon" | "no_icon" => el.unwrap()
          case cellContent if cellContent contains "ndla_table_cell_content" =>
            el.unwrap()
          case cell if cell contains "ndla_table_cell" => replaceTag(el, "td")
          case row if row contains "ndla_table_row"    => replaceTag(el, "tr")
          case table if table contains "ndla_table"    => replaceTag(el, "table")
          case _                                       => el.removeAttr("class")
        }
      }
    }

    def convertHeadings(el: Element): Unit = {
      for (el <- el.select("h1, h2, h3, h4, h5, h6").asScala) {
        el.className() match {
          case "frame" => replaceTagWithClass(el, "div", "c-bodybox")
          case _       => el
        }
      }
    }

    private def convertPres(el: Element): Unit = {
      for (el <- el.select("pre").asScala) {
        el.html("<code>" + el.html() + "</code>")
      }
    }

    private def convertSpans(element: Element): Unit = {
      unwrapOldChineseSpans(element)
      wrapChineseTextInSpans(element)
      setLanguageParameterIfPresent(element)
    }

    private def unwrapOldChineseSpans(element: Element): Unit = {
      element
        .select("span[style~=font-size]")
        .asScala
        .filter(tag => containsChineseText(tag.text))
        .foreach(el => el.unwrap())
    }

    private def wrapChineseTextInSpans(element: Element): Unit = {
      element
        .select("*")
        .asScala
        .filter(t => containsChineseText(t.ownText()))
        .foreach(t => {
          t.childNodes()
            .asScala
            .foreach(c => {
              if (c.nodeName() == "#text" && containsChineseText(c.outerHtml())) {
                // Need a temporary element to contain text and eventual spans
                val newNode = new Element("ElementToUnwrap")
                newNode.html(wrapChineseText(c.outerHtml()))
                c.replaceWith(newNode)
              }
            })
          t.select("ElementToUnwrap").unwrap()
        })
    }

    private def wrapChineseText(text: String): String = {
      // Split up string into parts that might be wrapped in spans if they contain chinese
      val splitString = text.foldLeft((List(""), false)) {
        case ((acc, spanWasOpen), chr) =>
          if ((isChineseCharacter(chr) && !spanWasOpen) || (!isChineseCharacter(chr) && spanWasOpen))
            (acc :+ chr.toString, !spanWasOpen)
          else
            (acc.init :+ acc.last + chr, spanWasOpen)
      }

      // Wrap the splits that actually contain chinese
      splitString._1
        .map(textPart => if (containsChineseText(textPart)) s"""<span lang="zh">$textPart</span>""" else textPart)
        .mkString
    }

    private def isChineseCharacter(c: Char, includeCommon: Boolean = true): Boolean = {
      val t = Character.UnicodeScript.of(c)
      t == Character.UnicodeScript.HAN || (t == Character.UnicodeScript.COMMON && includeCommon)
    }

    private def setLanguageParameterIfPresent(element: Element): Unit = {
      element
        .select("span")
        .asScala
        .foreach(spanTag => {
          val langAttribute = spanTag.attr("xml:lang")
          if (langAttribute.nonEmpty) {
            spanTag.attr("lang", langAttribute)
            spanTag.removeAttr("xml:lang")
          }
        })
    }

    private def handle_hide(el: Element): Unit = {
      replaceTag(el, "details")
      el.select("a.re-collapse").remove()
      val details = el.select("div.details").html() // save content
      el.select("div.details").remove()
      val summary = el.text()
      el.html(s"<summary>$summary</summary>")
      el.append(details)
    }

    private def replaceTagWithClass(el: Element, replacementTag: String, className: String): Unit = {
      replaceTag(el, replacementTag)
      el.addClass(className)
    }

    private def replaceTag(el: Element, replacementTag: String): Unit = {
      el.tagName(replacementTag)
      el.removeAttr("class")
    }

    private def containsChineseText(text: String): Boolean = {
      text
        .codePoints()
        .anyMatch(codepoint => Character.UnicodeScript.of(codepoint) == Character.UnicodeScript.HAN)
    }

    private def replaceAttribute(el: Element, originalAttrKey: String, attr: (String, String)): Unit = {
      val (attrKey, attrVal) = attr
      el.removeAttr(originalAttrKey)
      el.attr(attrKey, attrVal)
    }

  }
}
