/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters

import com.typesafe.scalalogging.LazyLogging
import no.ndla.articleimport.integration.ConverterModule.{jsoupDocumentToString, stringToJsoupDocument}
import no.ndla.articleimport.integration.{ConverterModule, ImageApiClient, LanguageContent, LanguageIngress}
import no.ndla.articleimport.model.domain.ImportStatus
import no.ndla.validation.EmbedTagRules.ResourceHtmlEmbedTag
import no.ndla.validation.{HtmlTagRules, ResourceType, TagAttributes, TagValidator}
import org.apache.commons.text.translate.{AggregateTranslator, EntityArrays, LookupTranslator}
import org.jsoup.nodes.{Element, Node, TextNode}

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.util.{Success, Try}

trait HTMLCleaner {
  this: ImageApiClient with HtmlTagGenerator =>
  val htmlCleaner: HTMLCleaner

  class HTMLCleaner extends ConverterModule with LazyLogging {

    override def convert(content: LanguageContent, importStatus: ImportStatus): Try[(LanguageContent, ImportStatus)] = {
      val element = stringToJsoupDocument(content.content)
      convertH5sToPStrong(element)
      val illegalTags = unwrapIllegalTags(element)
        .map(x => s"Illegal tag(s) removed: $x")
        .distinct
      convertLists(element)
      val illegalAttributes = removeAttributes(element)
        .map(x => s"Illegal attribute(s) removed: $x")
        .distinct

      removeEmptySpansWithoutAttributes(element)
      moveEmbedsOutOfPTags(element)
      removeComments(element)
      removeNbsp(element)
      wrapStandaloneTextInPTag(element)
      replaceNestedSections(element)

      val metaDescription = prepareMetaDescription(content.metaDescription)
      mergeTwoFirstSectionsIfFeasible(element)
      val ingress = getIngress(content, element)
      mergeTwoFirstSectionsIfFeasible(element)

      moveMisplacedAsideTags(element)
      unwrapDivsAroundDetailSummaryBox(element)
      unwrapDivsInAsideTags(element)
      unwrapNestedPs(element)

      convertH3sToH2s(element)
      convertAsideH2sToH1(element)
      val finalCleanedDocument = allContentMustBeWrappedInSectionBlocks(element)

      // Jsoup doesn't support removing elements while iterating the dom-tree.
      // Thus executes the routine 3 times in order to be sure to remove all tags
      (1 to 3).foreach(_ => removeEmptyTags(element))
      Success(
        (content.copy(content = jsoupDocumentToString(finalCleanedDocument),
                      metaDescription = metaDescription,
                      ingress = ingress),
         importStatus.addMessages(illegalTags ++ illegalAttributes)))
    }

    private def convertH5sToPStrong(element: Element): Unit = {
      element.select("h5").asScala.foreach(_.tagName("strong").wrap("p"))
    }

    private def convertAsideH2sToH1(element: Element): Unit = {
      element.select("aside h2").asScala.foreach(_.tagName("h1"))
    }

    private def convertH3sToH2s(element: Element) {
      if (element.select("h2").size() == 0)
        element.select("h3").asScala.foreach(_.tagName("h2"))
    }

    private def unwrapDivsAroundDetailSummaryBox(element: Element) {
      @tailrec
      def unwrapNestedDivs(detailsElem: Element) {
        if (detailsElem.parent.tagName == "div" && detailsElem.siblingElements.size == 0) {
          detailsElem.parent.unwrap()
          unwrapNestedDivs(detailsElem)
        }
      }

      element.select("details").asScala.foreach(unwrapNestedDivs)
    }

    private def tagIsValid(el: Element): Boolean = {
      new TagValidator().validateHtmlTag("content", el).isEmpty
    }

    private def removeEmptySpansWithoutAttributes(el: Element): Unit = {
      el.select("span")
        .asScala
        .filterNot(tag => tagIsValid(tag) && tag.attributes().asScala.nonEmpty)
        .foreach(_.unwrap)
    }

    private def moveEmbedsOutOfPTags(element: Element) {
      val embedsThatShouldNotBeInPTags = Set(
        ResourceType.Audio,
        ResourceType.Brightcove,
        ResourceType.ExternalContent,
        ResourceType.IframeContent,
        ResourceType.Image
      )

      val embedTypeString = embedsThatShouldNotBeInPTags
        .map(t => s"[${TagAttributes.DataResource}=$t]")
        .mkString(",")

      element
        .select("p")
        .asScala
        .foreach(pTag => {
          pTag
            .select(s"$ResourceHtmlEmbedTag$embedTypeString")
            .asScala
            .toList
            .foreach(el => {
              pTag.before(el.outerHtml())
              el.remove()
            })
        })
    }

    private def mergeTwoFirstSectionsIfFeasible(el: Element) {
      val sections = el.select("section").asScala

      if (sections.size < 2)
        return

      val firstSectionChildren = sections.head.children
      if (firstSectionChildren.size != 1 || firstSectionChildren.asScala.head.children.size > 2)
        return

      firstSectionChildren
        .select(ResourceHtmlEmbedTag)
        .asScala
        .headOption match {
        case Some(e) =>
          sections(1).prepend(e.outerHtml())
          e.remove()
          sections.head.childNodeSize() match {
            case x if x == 0 => sections.head.remove()
            case _           =>
          }
        case _ =>
      }
    }

    private def getIngress(content: LanguageContent, element: Element): Option[LanguageIngress] = {
      content.ingress match {
        case None => extractIngress(element).map(LanguageIngress(_, None))
        case Some(ingress) =>
          val imageEmbedHtml = ingress.ingressImage
            .flatMap(imageApiClient.importImage)
            .map(imageMetaData =>
              HtmlTagGenerator.buildImageEmbedContent(
                caption = "",
                imageId = imageMetaData.id.toString,
                align = "",
                size = "",
                altText = imageMetaData.alttext
                  .find(_.language == content.language)
                  .map(_.alttext)
                  .getOrElse("")
            ))

          imageEmbedHtml.map(element.prepend)
          Some(ingress.copy(content = extractElement(stringToJsoupDocument(ingress.content))))
      }
    }

    private def unwrapIllegalTags(el: Element): Seq[String] = {
      el.children()
        .select("*")
        .asScala
        .toList
        .filter(htmlTag => !HtmlTagRules.isTagValid(htmlTag.tagName))
        .map(illegalHtmlTag => {
          val tagName = illegalHtmlTag.tagName
          illegalHtmlTag.unwrap()
          tagName
        })
        .distinct
    }

    private def escapeHtml(text: String): String = {
      val escapeHtml = new AggregateTranslator(
        new LookupTranslator(EntityArrays.BASIC_ESCAPE),
        new LookupTranslator(EntityArrays.HTML40_EXTENDED_ESCAPE)
      )

      escapeHtml.translate(text)
    }

    private def prepareMetaDescription(metaDescription: String): String = {
      val element = stringToJsoupDocument(metaDescription)
      for (el <- element.select("embed").asScala) {
        val caption = el.attr("data-caption")
        el.replaceWith(new TextNode(caption))
      }
      escapeHtml(extractElement(element).trim)
    }

    private def removeAttributes(el: Element): Seq[String] = {
      el.select("*")
        .asScala
        .toList
        .flatMap(tag => HtmlTagRules.removeIllegalAttributes(tag, HtmlTagRules.legalAttributesForTag(tag.tagName)))
    }

    private def removeComments(node: Node) {
      var i = 0

      while (i < node.childNodeSize()) {
        val child = node.childNode(i)

        child.nodeName() == "#comment" || child.nodeName() == "#data" match {
          case true => child.remove()
          case false => {
            i += 1
            removeComments(child)
          }
        }
      }
    }

    private def htmlTagIsEmpty(el: Element) = {
      el.select(ResourceHtmlEmbedTag).isEmpty && el
        .select("math")
        .isEmpty && !el.hasText
    }

    private def removeEmptyTags(element: Element): Element = {
      val tagsToRemove = Set("p", "div", "section", "aside", "strong")
      for (el <- element.select(tagsToRemove.mkString(",")).asScala) {
        if (htmlTagIsEmpty(el)) {
          el.remove()
        }
      }

      element
    }

    // Since jsoup does not provide a way to remove &nbsp; from a tag, but not its children
    // We first replace it with a placeholder to then replace replace the placeholder with &nbsp;
    // in tags where nbsp's are allowed.
    private def removeNbsp(el: Element) {
      el.select("*")
        .select("mo")
        .asScala
        .foreach(mo => if (mo.html().equals(NBSP)) mo.html("[mathspace]"))
      el.html(el.html().replace(NBSP, " "))
      el.select("*")
        .select("mo")
        .asScala
        .foreach(mo => if (mo.html().equals("[mathspace]")) mo.html(NBSP))
    }

    // A paragraph containing an ingress can also be split up into mulitple strong-tags
    // e.g <p><strong>first</strong> <em><strong>second</strong></em></p>.
    // returns a sequence of all ingress elements
    private def getAllIngressElements(el: Element): Seq[Element] = {
      val paragraph = if (el.tagName == "p") el else el.parent

      if (paragraph.select("strong").text == paragraph.text)
        paragraph.select("strong").asScala
      else
        Seq(el)
    }

    private def getIngressText(el: Element): Option[Seq[Element]] = {
      val firstParagraphs = Option(el.select(">p"))
        .map(_.asScala.toList)
        .flatMap(paragraphs =>
          paragraphs.headOption
            .map(_ => paragraphs.take(2))) // select two first paragraphs
      val ingress = firstParagraphs.flatMap(ps => {
        val ingresses = ps.map(p => Option(p.select(">strong").first))

        // In some cases the ingress is split up into two paragraphs
        ingresses match {
          case Some(head) :: Some(second) :: _ =>
            Some(getAllIngressElements(head) ++ getAllIngressElements(second))
          case Some(head) :: None :: _ => Some(getAllIngressElements(head))
          case Some(head) :: _         => Some(getAllIngressElements(head))
          case None :: Some(second) :: _ =>
            ps.head.select(">embed").first match {
              case _: Element => Some(getAllIngressElements(second))
              case _          => None
            }

          case _ => None
        }
      })

      ingress match {
        case None =>
          Option(el.select(">strong:eq(0)").first)
            .orElse(Option(el.select(">strong:eq(1)").first))
            .map(Seq(_))
        case x => x
      }
    }

    private def findElementWithText(els: Seq[Element], tagName: String, text: String): Option[Element] = {
      for (el <- els)
        el.select(tagName)
          .asScala
          .find(t => t.tagName == tagName && t.text == text) match {
          case Some(e) => return Some(e)
          case None    =>
        }
      None
    }

    private def consecutiveNodesOfType(el: Element, tagName: String): Seq[Node] = {
      def canMerge(n: Node): Boolean =
        n != null && (n.nodeName() == tagName || n.toString == " ")
      Seq(el: Node) ++ el
        .siblingNodes()
        .asScala
        .drop(el.siblingIndex())
        .takeWhile(canMerge)
    }

    private def mergeConsecutiveTags(el: Element, tagName: String): Unit = {
      for (nodes <- consecutiveNodesOfType(el, tagName).drop(1)) {
        el.appendChild(nodes)

        if (nodes.nodeName() != "#text")
          nodes.unwrap()
      }
    }

    private def extractIngress(el: Element): Option[String] = {
      val minimumIngressWordCount = 3
      val strippedDownArticle = stringToJsoupDocument(el.html())
      val tagsToKeep = Set("p", "strong", "body", "embed", "section")
      val tagsToRemove = Set("aside")

      strippedDownArticle
        .select("*")
        .asScala
        .filter(e => tagsToRemove.contains(e.tagName))
        .foreach(_.remove())

      strippedDownArticle
        .select("*")
        .asScala
        .filterNot(e => tagsToKeep.contains(e.tagName))
        .foreach(_.unwrap())

      removeEmptyTags(strippedDownArticle)

      val firstP = Option(strippedDownArticle.select("body>section:eq(0)>p:lt(2)>strong").first())
        .map(_.parent)
      firstP.flatMap(p => {
        mergeConsecutiveTags(p, "p")
        val ingressTexts =
          consecutiveNodesOfType(p.select(">strong").first(), "strong").map {
            case s: Element => s.text
            case s          => s.toString
          }
        val ingressText = ingressTexts.mkString(" ").replaceAll(" +", " ")
        val articleStartsWithIngress =
          p.text().startsWith(ingressTexts.mkString(""))

        if (ingressText
              .split(" ")
              .length < minimumIngressWordCount || !articleStartsWithIngress) {
          None
        } else {
          ingressTexts.foreach(
            t =>
              findElementWithText(el.select("p").asScala, "strong", t)
                .map(_.remove))
          removeEmptyTags(el)
          Some(ingressText)
        }
      })
    }

    private def extractElement(elementToExtract: Element): String = {
      elementToExtract.remove()
      elementToExtract.text()
    }

    val NodeTypesToGroupTogether = "em" :: "#text" :: "math" :: "strong" :: Nil

    private def wrapThingsInP(nodes: Seq[Node]) {
      val grouped = new Element("p")

      val firstNonTextElementIdx = nodes.indexWhere(
        n =>
          !NodeTypesToGroupTogether
            .contains(n.nodeName()) && n.toString.trim.length > 0) match {
        case idx: Int if idx < 0 => nodes.length
        case idx                 => idx
      }

      val toBeWrapped = nodes.slice(0, firstNonTextElementIdx)
      toBeWrapped.foreach(child => grouped.appendChild(child.clone))
      toBeWrapped.drop(1).foreach(_.remove())
      nodes.headOption.foreach(_.replaceWith(grouped))
    }

    def wrapStandaloneTextInPTag(element: Element): Element = {
      val sections = element.select("body>section").asScala

      sections.foreach(section => {
        def firstTextNodeIdx: Int =
          section.childNodes.asScala.indexWhere(n => NodeTypesToGroupTogether.contains(n.nodeName()))

        while (firstTextNodeIdx > -1) {
          val childNodes = section.childNodes().asScala
          wrapThingsInP(childNodes.drop(firstTextNodeIdx))
        }
      })

      element
    }

    private def unwrapDivsInAsideTags(element: Element): Element = {
      val asides = element.select("body>section>aside").asScala
      for (aside <- asides) {
        if (aside.children.size == 1)
          unwrapNestedDivs(aside.children.first)
      }
      element
    }

    private def unwrapNestedDivs(child: Element) {
      if (child.tagName() == "div" && child.siblingElements.size == 0) {
        val firstChild = Option(child.children.first)
        child.unwrap()
        firstChild match {
          case Some(c) => unwrapNestedDivs(c)
          case None    =>
        }
      }
    }

    private def unwrapNestedPs(element: Element): Unit = {
      val ps = element.select("p").asScala
      ps.foreach(p => {
        val childTags = p.children.asScala.map(_.tagName())
        if (childTags.contains("p"))
          p.unwrap()
      })
    }

    private def moveMisplacedAsideTags(element: Element) = {
      val aside =
        element.select("body>section:eq(0)>aside:eq(0)").asScala.headOption
      aside match {
        case None =>
        case Some(e) =>
          val sibling = e.siblingElements().asScala.lift(0)
          sibling.map(s => s.before(e))
      }
      element
    }

    private def allContentMustBeWrappedInSectionBlocks(element: Element): Element = {
      val body = element.select("body").first
      if (body.childNodeSize() < 1)
        return element

      val rootLevelBlocks = body.children
      if (rootLevelBlocks.select("section").isEmpty) {
        return stringToJsoupDocument(s"<section>${body.outerHtml}</section>")
      }

      if (rootLevelBlocks.first().tagName() != "section") {
        body.prepend("<section></section>")
      }

      rootLevelBlocks.asScala.foreach {
        case el if el.tagName != "section" =>
          el.previousElementSibling.append(el.outerHtml)
          el.remove()
        case _ =>
      }
      element
    }

    private def convertLists(element: Element) = {
      element
        .select("ol")
        .asScala
        .foreach(x => {
          val styling = x.attr("style").split(";")
          if (styling.contains("list-style-type: lower-alpha")) {
            x.attr(TagAttributes.DataType.toString, "letters")
          }
        })
    }

    private def replaceNestedSections(element: Element) = {
      element
        .select("section")
        .asScala
        .foreach(sec => {
          if (sec.parents().asScala.exists(p => p.tagName() == "section")) {
            sec.tagName("div")
          }
        })
    }

  }

}
