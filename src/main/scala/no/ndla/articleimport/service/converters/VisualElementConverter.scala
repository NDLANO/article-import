/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters

import no.ndla.articleimport.integration.ConverterModule.{jsoupDocumentToString, stringToJsoupDocument}
import no.ndla.articleimport.integration.{AudioApiClient, ConverterModule, ImageApiClient, LanguageContent}
import no.ndla.articleimport.model.api.ImportException
import no.ndla.articleimport.model.domain.{ImportStatus, RequiredLibrary}
import no.ndla.articleimport.service.ExtractService
import no.ndla.articleimport.service.converters.contentbrowser.{AudioConverterModule, H5PConverterModule, ImageConverterModule, VideoConverterModule}

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

trait VisualElementConverter {
  this: ExtractService
    with ImageApiClient
    with AudioApiClient
    with H5PConverterModule
    with ImageConverterModule
    with VideoConverterModule
    with AudioConverterModule =>

  object VisualElementConverter extends ConverterModule {
    def convert(content: LanguageContent, importStatus: ImportStatus): Try[(LanguageContent, ImportStatus)] = {
      if (content.visualElement.isEmpty)
        return Success((content, importStatus))

      content.visualElement.flatMap(nodeIdToVisualElement) match {
        case Some((visual, requiredLibs)) =>
          val requiredLibraries = content.requiredLibraries ++ requiredLibs
          Success(content.copy(content = removeVisualElementFromContent(content.content, visual), visualElement=Some(visual), requiredLibraries=requiredLibraries), importStatus)
        case None => Failure(ImportException(s"Failed to convert node id ${content.visualElement.get}"))
      }
    }

    private def removeVisualElementFromContent(content: String, visualElement: String): String = {
      val element = stringToJsoupDocument(content)
      val visualEmbed = stringToJsoupDocument(visualElement).select("embed").first()
      element.select("embed").asScala.foreach(contentEmbed => {
        if (contentEmbed.toString == visualEmbed.toString) {
          contentEmbed.remove()
        }
      })
      jsoupDocumentToString(element)
    }

    private def nodeIdToVisualElement(nodeId: String): Option[(String, Seq[RequiredLibrary])] = {
      val converters = Map[String, String => Option[(String, Seq[RequiredLibrary])]](
        ImageConverter.typeName -> toImage,
        AudioConverter.typeName -> toAudio,
        H5PConverter.typeName -> toH5P,
        VideoConverter.typeName -> toVideo
      )

      extractService.getNodeType(nodeId).flatMap(nodeType => {
        converters.get(nodeType.toLowerCase).flatMap(func => func(nodeId))
      })
    }

    private def toImage(nodeId: String): Option[(String, Seq[RequiredLibrary])] =
      ImageConverter.toImageEmbed(nodeId, "", "", "", "").map(imageEmbed => (imageEmbed, Seq.empty)).toOption

    private def toH5P(nodeId: String): Option[(String, Seq[RequiredLibrary])] =
      H5PConverter.toH5PEmbed(nodeId).map(h5pEmbed => (h5pEmbed, Seq.empty)).toOption


    private def toVideo(nodeId: String): Option[(String, Seq[RequiredLibrary])] = Some((VideoConverter.toInlineVideo("", nodeId), Seq.empty))

    private def toAudio(nodeId: String): Option[(String, Seq[RequiredLibrary])] =
      AudioConverter.toAudio(nodeId, "").map(audioEmbed => (audioEmbed, Seq.empty)).toOption

  }
}
