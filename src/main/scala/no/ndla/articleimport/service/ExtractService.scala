/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service

import com.typesafe.scalalogging.LazyLogging
import no.ndla.articleimport.integration._
import no.ndla.articleimport.model.domain.{BiblioMeta, ContentFilMeta, NodeGeneralContent, NodeToConvert}
import no.ndla.articleimport.integration.ConverterModule.stringToJsoupDocument

import scala.util.{Failure, Success, Try}

trait ExtractService {
  this: MigrationApiClient with TagsService =>

  val extractService: ExtractService

  class ExtractService extends LazyLogging {

    def getNodeData(nodeId: String): Try[NodeToConvert] = {
      val tagsForNode = tagsService.forContent(nodeId) match {
        case Failure(e) =>
          logger.warn(s"Could not import tags for node $nodeId", e)
          List()
        case Success(tags) => tags
      }

      migrationApiClient.getContentNodeData(nodeId) match {
        case Success(data) => Success(data.asNodeToConvert(nodeId, tagsForNode))
        case Failure(ex) =>
          logger.error(s"Getting content from migration-api failed with error: ${ex.getMessage}")
          Failure(ex)
      }
    }

    def getNodeType(nodeId: String): Option[String] =
      migrationApiClient.getContentType(nodeId).map(x => x.nodeType).toOption

    def getNodeEmbedMeta(nodeId: String): Try[MigrationEmbedMeta] =
      migrationApiClient.getNodeEmbedData(nodeId)

    def getNodeFilMeta(nodeId: String): Try[Seq[ContentFilMeta]] =
      migrationApiClient.getFilMeta(nodeId).map(_.map(_.asContentFilMeta))

    def getNodeGeneralContent(nodeId: String): Seq[NodeGeneralContent] = {
      val content = migrationApiClient
        .getNodeGeneralContent(nodeId)
        .getOrElse(Seq())
        .map(x => x.asNodeGeneralContent)

      // make sure to return the content along with all its translations
      content.exists { x =>
        x.isMainNode
      } match {
        case true => content
        case false =>
          if (content.nonEmpty)
            migrationApiClient
              .getNodeGeneralContent(content.head.tnid)
              .getOrElse(Seq())
              .map(x => x.asNodeGeneralContent)
          else
            content
      }
    }

    def getBiblioMeta(nodeId: String): Option[BiblioMeta] =
      migrationApiClient.getBiblioMeta(nodeId).map(x => x.asBiblioMeta).toOption

    def getLinkEmbedMeta(externalId: String): Try[MigrationEmbedMeta] = {
      extractService
        .getNodeEmbedMeta(externalId)
        .map(meta => meta.copy(url = meta.url.orElse(tryFetchSrcAttributeFromTag(meta.embedCode.getOrElse("")))))
    }

    private def tryFetchSrcAttributeFromTag(tag: String): Option[String] = {
      Option(stringToJsoupDocument(tag).select("[src]").attr("src"))
        .filter(_.trim.nonEmpty)
    }
  }
}
