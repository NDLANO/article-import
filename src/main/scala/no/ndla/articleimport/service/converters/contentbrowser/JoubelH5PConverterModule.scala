/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.articleimport.service.converters.contentbrowser

import com.typesafe.scalalogging.LazyLogging
import no.ndla.articleimport.model.domain.{ImportStatus, RequiredLibrary}
import no.ndla.articleimport.service.ExtractService
import no.ndla.articleimport.service.converters.HtmlTagGenerator

import scala.util.{Success, Try}

trait JoubelH5PConverterModule {
  this: ExtractService with HtmlTagGenerator =>

  object JoubelH5PConverter extends ContentBrowserConverterModule with LazyLogging {
    override val typeName: String = "h5p_content"
    val JoubelH5PBaseUrl = "https://ndlah5p.joubel.com/node"

    override def convert(content: ContentBrowser, importStatus: ImportStatus): Try[(String, Seq[RequiredLibrary], ImportStatus)] = {
      val ndlaNodeId = content.get("nid")

      ValidH5PNodeIds.get(ndlaNodeId) match {
        case Some(joubelNodeId) => Success(validH5PResource(joubelNodeId, content),
          Seq(), importStatus)
        case None => Success(invalidH5PResource(ndlaNodeId, content, importStatus))
      }
    }

    private def validH5PResource(joubelH5PNodeId: String, content: ContentBrowser) = {
      val ndlaNodeId = content.get("nid")
      logger.info(s"Converting h5p_content with nid $ndlaNodeId")
      HtmlTagGenerator.buildH5PEmbedContent(s"$JoubelH5PBaseUrl/${ValidH5PNodeIds(ndlaNodeId)}")
    }

    private def invalidH5PResource(nodeId: String, content: ContentBrowser, importStatus: ImportStatus): (String, Seq[RequiredLibrary], ImportStatus) = {
      val ndlaNodeId = content.get("nid")
      val message = s"H5P node $ndlaNodeId is not yet exported to new H5P service"
      logger.error(message)

      val replacement = HtmlTagGenerator.buildErrorContent(message)
      (replacement, Seq(), importStatus.addMessage(message))
    }

  }

  private[contentbrowser] val ValidH5PNodeIds = Map(
    "160303" -> "1",
    "166925" -> "2",
    "157946" -> "3",
    "159786" -> "4",
    "158769" -> "5",
    "158794" -> "6",
    "163366" -> "7",
    "169536" -> "8",
    "158729" -> "9",
    "169894" -> "10",
    "155485" -> "11",
    "160176" -> "12",
    "162644" -> "13",
    "164475" -> "14",
    "167619" -> "15",
    "170366" -> "16",
    "160127" -> "17",
    "156653" -> "18",
    "167124" -> "19",
    "162262" -> "20",
    "160779" -> "21",
    "157945" -> "22",
    "160666" -> "23",
    "168908" -> "24",
    "161082" -> "25"
  )

}
