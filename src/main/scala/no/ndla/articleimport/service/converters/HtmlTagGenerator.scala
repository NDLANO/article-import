/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters

import no.ndla.validation.{ResourceType, TagAttributes}
import no.ndla.validation.EmbedTagRules.ResourceHtmlEmbedTag

trait HtmlTagGenerator {

  object HtmlTagGenerator {
    def buildEmbedContent(dataAttributes: Map[TagAttributes.Value, String]): String = {
      s"<$ResourceHtmlEmbedTag ${buildAttributesString(dataAttributes)} />"
    }

    def buildErrorContent(message: String): String =
      buildEmbedContent(Map(TagAttributes.DataResource -> ResourceType.Error.toString, TagAttributes.DataMessage -> message))

    def buildImageEmbedContent(caption: String, imageId: String, align: String, size: String, altText: String) = {
      val dataAttributes = Map(
        TagAttributes.DataResource -> ResourceType.Image.toString,
        TagAttributes.DataResource_Id -> imageId,
        TagAttributes.DataSize -> size,
        TagAttributes.DataAlt -> altText,
        TagAttributes.DataCaption -> caption,
        TagAttributes.DataAlign -> align)

      buildEmbedContent(dataAttributes)
    }

    def buildAudioEmbedContent(audioId: String, caption: String) = {
      val dataAttributes = Map(
        TagAttributes.DataResource -> ResourceType.Audio.toString,
        TagAttributes.DataResource_Id -> audioId,
        TagAttributes.DataCaption -> caption
      )
      buildEmbedContent(dataAttributes)
    }

    def buildH5PEmbedContent(url: String) = {
      val dataAttributes = Map(TagAttributes.DataResource -> ResourceType.H5P.toString, TagAttributes.DataUrl -> url)
      buildEmbedContent(dataAttributes)
    }

    def buildBrightCoveEmbedContent(caption: String, videoId: String, account: String, player: String) = {
      val dataAttributes = Map(
        TagAttributes.DataResource -> ResourceType.Brightcove.toString,
        TagAttributes.DataCaption -> caption,
        TagAttributes.DataVideoId -> videoId,
        TagAttributes.DataAccount -> account,
        TagAttributes.DataPlayer -> player
      )
      buildEmbedContent(dataAttributes)
    }

    def buildContentLinkEmbedContent(contentId: Long, linkText: String, openInNewWindow: Boolean): String = {
      val attributes = Map(
        TagAttributes.DataResource -> ResourceType.ContentLink.toString,
        TagAttributes.DataContentId -> s"$contentId",
        TagAttributes.DataLinkText -> linkText)

      val openInNewWindowAttr =
        if (openInNewWindow)
          Seq(TagAttributes.DataOpenIn -> "new-context")
        else
          Seq.empty

      buildEmbedContent(attributes ++ openInNewWindowAttr)
    }

    def buildConceptEmbedContent(conceptId: Long, linkText: String) = {
      val dataAttributes = Map(
        TagAttributes.DataResource -> ResourceType.ConceptLink.toString,
        TagAttributes.DataContentId -> s"$conceptId",
        TagAttributes.DataLinkText -> linkText)
      buildEmbedContent(dataAttributes)
    }

    def buildExternalInlineEmbedContent(url: String) = {
      val dataAttributes = Map(
        TagAttributes.DataResource -> ResourceType.ExternalContent.toString,
        TagAttributes.DataUrl -> url
      )
      buildEmbedContent(dataAttributes)
    }

    def buildNRKInlineVideoContent(nrkVideoId: String, url: String) = {
      val dataAttributes = Map(
        TagAttributes.DataResource -> ResourceType.NRKContent.toString,
        TagAttributes.DataNRKVideoId -> nrkVideoId,
        TagAttributes.DataUrl -> url
      )
      buildEmbedContent(dataAttributes)
    }

    def buildRegularInlineContent(url: String, width: String, height: String, resourceType: ResourceType.Value) = {
      val dataAttributes = Map(
        TagAttributes.DataResource -> resourceType.toString,
        TagAttributes.DataUrl -> url,
        TagAttributes.DataWidth -> width,
        TagAttributes.DataHeight -> height
      )
      buildEmbedContent(dataAttributes)
    }

    def buildDetailsSummaryContent(linkText: String, content: String) = {
      s"<details><summary>$linkText</summary>$content</details>"
    }

    def buildAnchor(href: String, anchorText: String, title: String, openInNewTab: Boolean): String = {
      val target = openInNewTab match {
        case true => Map(TagAttributes.Target -> "_blank", TagAttributes.Rel -> "noopener noreferrer")
        case false => Map[TagAttributes.Value, String]()
      }
      val attributes = Map(TagAttributes.Href -> href, TagAttributes.Title -> title) ++ target
      s"<a ${buildAttributesString(attributes)}>$anchorText</a>"
    }

    def buildFootNoteItem(title: String, `type`: String, year: String, edition: String, publisher: String, authors: Set[String]) = {
      val attrs = Map(
        TagAttributes.DataResource -> ResourceType.FootNote.toString,
        TagAttributes.DataTitle -> title,
        TagAttributes.DataType -> `type`,
        TagAttributes.DataYear -> year,
        TagAttributes.DataEdition -> edition,
        TagAttributes.DataPublisher -> publisher,
        TagAttributes.DataAuthors -> authors.mkString(";")
      )
      buildEmbedContent(attrs)
    }

    def buildRelatedContent(articleIds: Set[Long]): String = {
      val attrs = Map(
        TagAttributes.DataResource -> ResourceType.RelatedContent.toString,
        TagAttributes.DataArticleIds -> articleIds.map(_.toString).mkString(",")
      )
      buildEmbedContent(attrs)
    }

    private def buildAttributesString(figureDataAttributeMap: Map[TagAttributes.Value, String]): String =
      figureDataAttributeMap.toList.sortBy(_._1.toString).map { case (key, value) => s"""$key="${value.trim.replace("\"", "&quot;")}"""" }.mkString(" ")

  }

}

