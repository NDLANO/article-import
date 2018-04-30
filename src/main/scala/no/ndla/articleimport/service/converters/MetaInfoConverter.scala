/*
 * Part of NDLA article_import.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters

import com.typesafe.scalalogging.LazyLogging
import no.ndla.articleimport.ArticleImportProperties.nodeTypeLink
import no.ndla.articleimport.integration.ImageApiClient
import no.ndla.articleimport.model.domain.{ArticleMetaImage, NodeWithConvertedMeta, ImportStatus, NodeToConvert}

trait MetaInfoConverter {
  this: ImageApiClient =>

  object MetaInfoConverter extends LazyLogging {

    def convert(nodeToConvert: NodeToConvert, importStatus: ImportStatus): (NodeWithConvertedMeta, ImportStatus) = {

      val license = nodeToConvert.license match {
        case Some(l) => l
        case None    => if (nodeToConvert.nodeType == nodeTypeLink) "by-sa" else ""
      }

      (NodeWithConvertedMeta(
         titles = nodeToConvert.titles,
         contents = nodeToConvert.contents,
         license,
         authors = nodeToConvert.authors,
         tags = nodeToConvert.tags,
         nodeType = nodeToConvert.nodeType,
         contentType = nodeToConvert.contentType,
         created = nodeToConvert.created,
         updated = nodeToConvert.updated,
         metaImages = getMetaImages(nodeToConvert),
         articleType = nodeToConvert.articleType,
         editorialKeywords = nodeToConvert.editorialKeywords
       ),
       importStatus)
    }

    private def getMetaImages(nodeToConvert: NodeToConvert): Seq[ArticleMetaImage] = {
      nodeToConvert.contents
        .flatMap(c =>
          c.metaImage.map(imageNid =>
            imageApiClient.importImage(imageNid) match {
              case Some(image) =>
                Some(ArticleMetaImage(image.id, c.language))
              case None =>
                logger.warn(s"Failed to import meta image with node id $imageNid")
                None
          }))
        .flatten
    }

  }

}
