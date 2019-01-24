/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters

import java.util.UUID.randomUUID

import no.ndla.articleimport.integration.ConverterModule.stringToJsoupDocument
import no.ndla.articleimport.integration.{AudioApiClient, ConverterModule, ImageApiClient, LanguageContent}
import no.ndla.articleimport.model.api.ImportException
import no.ndla.articleimport.model.domain.{ArticleType, ImportStatus, RequiredLibrary}
import no.ndla.articleimport.service.ExtractService
import no.ndla.articleimport.service.converters.contentbrowser.{
  AudioConverterModule,
  H5PConverterModule,
  ImageConverterModule,
  VideoConverterModule
}
import org.jsoup.nodes.TextNode

import scala.util.{Success, Try}

trait VisualElementConverter {
  this: ExtractService
    with HtmlTagGenerator
    with ImageApiClient
    with AudioApiClient
    with H5PConverterModule
    with ImageConverterModule
    with VideoConverterModule
    with AudioConverterModule =>

  object VisualElementConverter extends ConverterModule {

    def convert(content: LanguageContent, importStatus: ImportStatus): Try[(LanguageContent, ImportStatus)] = {

      // Only 'topic-article's should attempt to extract visual element
      val (newContent, newEmbed) =
        if (importStatus.articleType == ArticleType.TopicArticle) contentAndVisualElementFromContent(content.content)
        else (content.content, None)

      content.visualElement match {
        case Some(visualElementNodeId) =>
          nodeIdToVisualElement(visualElementNodeId) match {
            case Some((visual, requiredLibs)) =>
              val requiredLibraries = content.requiredLibraries ++ requiredLibs
              Success(content.copy(
                        content = newContent,
                        visualElement = Some(visual),
                        requiredLibraries = requiredLibraries
                      ),
                      importStatus)
            case None =>
              val status = importStatus.addError(
                ImportException(content.nid,
                                s"Failed to convert visual element node ${content.visualElement.getOrElse("")}"))
              val visual = newEmbed.getOrElse(HtmlTagGenerator.buildErrorContent("Innhold mangler."))
              Success((content.copy(content = newContent, visualElement = Some(visual)), status))
          }
        case None =>
          Success(content.copy(content = newContent, visualElement = newEmbed), importStatus)
      }
    }

    /** Attempts to extract any embed appearing before text returns (contentWithoutEmbed, extractedEmbed) */
    private[service] def contentAndVisualElementFromContent(content: String): (String, Option[String]) = {
      val element = stringToJsoupDocument(content)
      val firstEmbed = Option(element.select("embed").first())

      val shouldExtract = firstEmbed match {
        case Some(embed) =>
          val uuidStr = randomUUID().toString
          val textNode = new TextNode(uuidStr)
          embed.before(textNode)
          val embedIsBeforeText = element.text().startsWith(uuidStr)
          textNode.remove()
          embedIsBeforeText
        case None => false
      }

      val extractedEmbed = if (shouldExtract) {
        firstEmbed.map(e => {
          val html = e.outerHtml()
          e.remove()
          html
        })
      } else { None }

      (element.html(), extractedEmbed)
    }

    private def nodeIdToVisualElement(nodeId: String): Option[(String, Seq[RequiredLibrary])] = {
      val converters =
        Map[String, String => Option[(String, Seq[RequiredLibrary])]](
          ImageConverterModule.typeName -> toImage,
          AudioConverterModule.typeName -> toAudio,
          H5PConverterModule.typeName -> toH5P,
          VideoConverterModule.typeName -> toVideo
        )

      extractService
        .getNodeType(nodeId)
        .flatMap(nodeType => {
          converters.get(nodeType.toLowerCase).flatMap(func => func(nodeId))
        })
    }

    private def toImage(nodeId: String): Option[(String, Seq[RequiredLibrary])] =
      ImageConverterModule
        .toImageEmbed(nodeId, "", "", "", "")
        .map(imageEmbed => (imageEmbed, Seq.empty))
        .toOption

    private def toH5P(nodeId: String): Option[(String, Seq[RequiredLibrary])] =
      H5PConverterModule
        .toH5PEmbed(nodeId)
        .map(h5pEmbed => (h5pEmbed, Seq.empty))
        .toOption

    private def toVideo(nodeId: String): Option[(String, Seq[RequiredLibrary])] =
      Some((VideoConverterModule.toInlineVideo("", nodeId), Seq.empty))

    private def toAudio(nodeId: String): Option[(String, Seq[RequiredLibrary])] =
      AudioConverterModule
        .toAudio(nodeId, "", false)
        .map(audioEmbed => (audioEmbed, Seq.empty))
        .toOption

  }
}
