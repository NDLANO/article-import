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
import no.ndla.articleimport.model.api.ImportException
import no.ndla.articleimport.model.domain.{ArticleMetaImage, ImportStatus, NodeToConvert, NodeWithConvertedMeta}

import scala.util.{Failure, Success, Try}

trait MetaInfoConverter {
  this: ImageApiClient =>

  object MetaInfoConverter extends LazyLogging {

    def convert(nodeToConvert: NodeToConvert,
                importStatus: ImportStatus): Try[(NodeWithConvertedMeta, ImportStatus)] = {

      combineLicenses(nodeToConvert, importStatus).map {
        case (license, updatedStatus) =>
          val authors = combineAuthors(nodeToConvert, updatedStatus)

          (NodeWithConvertedMeta(
             titles = nodeToConvert.titles,
             contents = nodeToConvert.contents,
             license = license,
             authors = authors.toSeq,
             tags = nodeToConvert.tags,
             nodeType = nodeToConvert.nodeType,
             contentType = nodeToConvert.contentType,
             created = nodeToConvert.created,
             updated = nodeToConvert.updated,
             metaImages = getMetaImages(nodeToConvert),
             articleType = nodeToConvert.articleType,
             editorialKeywords = nodeToConvert.editorialKeywords
           ),
           updatedStatus)
      }

    }

    private def combineAuthors(nodeToConvert: NodeToConvert, importStatus: ImportStatus) =
      (nodeToConvert.authors ++ importStatus.insertedAuthors).toSet

    private def combineLicenses(nodeToConvert: NodeToConvert,
                                importStatus: ImportStatus): Try[(String, ImportStatus)] = {

      val license = nodeToConvert.license match {
        case Some(l) => l
        case None    => if (nodeToConvert.nodeType == nodeTypeLink) "by-sa" else ""
      }

      val mainNid = nodeToConvert.getMainNid.orElse(nodeToConvert.getNid).getOrElse("")
      val allLicenses = (importStatus.insertedLicenses :+ license).toSet

      val combinableLicenses = Map(
        Set("by-sa", "by-nc-sa") -> "by-nc-sa"
      )

      combinableLicenses.get(allLicenses) match {
        case Some(combinedLicense) =>
          Success(
            (combinedLicense,
             importStatus.addMessage(s"Successfully combined licenses: $allLicenses into $combinedLicense")))
        case None if allLicenses.size == 1 =>
          Success((license, importStatus))
        case None =>
          val msg =
            s"Could not combine license $license with inserted licenses: ${importStatus.insertedLicenses.mkString(",")}."
          logger.error(msg)
          Failure(ImportException(mainNid, msg))
      }
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
