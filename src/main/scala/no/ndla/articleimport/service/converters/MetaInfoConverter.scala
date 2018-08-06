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
import no.ndla.articleimport.model.domain._
import no.ndla.mapping.License.getLicenses

import scala.util.{Failure, Success, Try}

trait MetaInfoConverter {
  this: ImageApiClient =>

  object MetaInfoConverter extends LazyLogging {

    def convert(nodeToConvert: NodeToConvert,
                importStatus: ImportStatus): Try[(NodeWithConvertedMeta, ImportStatus)] = {

      handleLicenses(nodeToConvert, importStatus).map {
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
      (nodeToConvert.authors ++ importStatus.nodeLocalContext.insertedAuthors).toSet

    private def handleLicenses(nodeToConvert: NodeToConvert,
                               importStatus: ImportStatus): Try[(String, ImportStatus)] = {

      val license = nodeToConvert.license match {
        case Some(l) => l
        case None    => if (nodeToConvert.nodeType == nodeTypeLink) "by-sa" else ""
      }

      val mainNid = nodeToConvert.getMainNid.orElse(nodeToConvert.getNid).getOrElse("")
      val allLicenses = (importStatus.nodeLocalContext.insertedLicenses :+ license).toSet

      if (allLicenses.size == 1) {
        Success((license, importStatus))
      } else {
        combineLicenses(allLicenses) match {
          case Some(combinedLicense) =>
            Success(
              (combinedLicense,
               importStatus.addMessage(s"Successfully combined licenses: $allLicenses into $combinedLicense")))
          case None =>
            val msg =
              s"Could not combine license $license with inserted licenses: ${importStatus.nodeLocalContext.insertedLicenses
                .mkString(",")}."
            logger.error(msg)
            Failure(ImportException(mainNid, msg))
        }
      }
    }

    /**
      * Combines set of cc licenses into a single cc license with components from all.
      * For example 'by-sa' and 'by-nc' is combined into 'by-nc-sa'
      *
      * @param licenses Set of cc licenses
      * @return Combined license
      */
    private def combineLicenses(licenses: Set[String]): Option[String] = {
      val usedParts = licenses.flatMap(license => license.split('-'))
      val combinedLicense = getLicenses.find(l => {
        usedParts.forall(part => {
          l.license.contains(part)
        })
      })

      combinedLicense.map(_.license)
    }

    private def getMetaImages(nodeToConvert: NodeToConvert): Seq[ArticleMetaImage] = {
      val imagesToImport = nodeToConvert.contents.flatMap(_.metaImage).distinct
      val importedImages = imagesToImport.flatMap(nid => {
        if (imageApiClient.importImage(nid).nonEmpty) {
          Some(nid)
        } else {
          logger.warn(s"Failed to import meta image with node id $nid")
          None
        }
      })

      nodeToConvert.contents.flatMap(content =>
        content.metaImage.flatMap(imageNid => {
          if (importedImages.contains(imageNid)) {
            imageApiClient
              .getMetaByExternId(imageNid, content.language)
              .map(imageInLanguage => {
                ArticleMetaImage(imageInLanguage.id,
                                 imageInLanguage.alttext.map(_.alttext).getOrElse(""),
                                 content.language)
              })
          } else None
        }))
    }
  }

}
