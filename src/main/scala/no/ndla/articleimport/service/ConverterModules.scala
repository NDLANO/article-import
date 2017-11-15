/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.articleimport.service

import no.ndla.articleimport.integration.ConverterModule
import no.ndla.articleimport.model.api.ImportExceptions
import no.ndla.articleimport.model.domain.{ImportStatus, NodeToConvert}
import no.ndla.articleimport.ArticleImportProperties.{nodeTypeBegrep, nodeTypeVideo, nodeTypeH5P}

import scala.util.{Failure, Success, Try}

case class ConverterPipeLine(mainConverters: Seq[ConverterModule], postProcessorConverters: Seq[ConverterModule])

trait ConverterModules {
  val articleConverter: ConverterPipeLine
  val conceptConverter: ConverterPipeLine
  val leafNodeConverter: ConverterPipeLine

  private lazy val Converters = Map(
    nodeTypeBegrep -> conceptConverter,
    nodeTypeH5P -> leafNodeConverter,
    nodeTypeVideo -> leafNodeConverter
  ).withDefaultValue(articleConverter)

  def executeConverterModules(nodeToConvert: NodeToConvert, importStatus: ImportStatus): Try[(NodeToConvert, ImportStatus)] =
    runConverters(Converters(nodeToConvert.nodeType.toLowerCase).mainConverters, nodeToConvert, importStatus)

  def executePostprocessorModules(nodeToConvert: NodeToConvert, importStatus: ImportStatus): Try[(NodeToConvert, ImportStatus)] =
    runConverters(Converters(nodeToConvert.nodeType.toLowerCase).postProcessorConverters, nodeToConvert, importStatus)

  private def runConverters(converters: Seq[ConverterModule], nodeToConvert: NodeToConvert, importStatus: ImportStatus): Try[(NodeToConvert, ImportStatus)] = {
     val (convertedNode, finalImportStatus, exceptions) = converters.foldLeft((nodeToConvert, importStatus, Seq[Throwable]()))((element, converter) => {

      val (partiallyConvertedNode, importStatus, exceptions) = element
      converter.convert(partiallyConvertedNode, importStatus) match {
        case Success((updatedNode, updatedImportStatus)) => (updatedNode, updatedImportStatus, exceptions)
        case Failure(x) => (partiallyConvertedNode, importStatus, exceptions :+ x)
      }
    })

    if (exceptions.nonEmpty) {
      val failedNodeIds = nodeToConvert.contents.map(_.nid).mkString(",")
      return Failure(new ImportExceptions(s"Error importing node(s) with id(s) $failedNodeIds", errors=exceptions))
    }

    Success((convertedNode, finalImportStatus))
  }


}
