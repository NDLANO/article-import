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
import no.ndla.articleimport.ArticleImportProperties.{nodeTypeBegrep, nodeTypeH5P, nodeTypeLink, nodeTypeVideo}
import no.ndla.articleimport.service.converters._
import no.ndla.articleimport.service.converters.contentbrowser.ContentBrowserConverter
import no.ndla.articleimport.service.converters.SimpleTagConverter

import scala.util.{Failure, Success, Try}

case class ConverterPipeLine(mainConverters: Seq[ConverterModule], postProcessorConverters: Seq[ConverterModule])

trait ConverterModules {
  this: ContentBrowserConverter
    with LeafNodeConverter
    with HTMLCleaner
    with FileDivConverter
    with VisualElementConverter
    with SimpleTagConverter
    with RelatedContentConverter =>

  lazy val articleConverter = ConverterPipeLine(
    mainConverters = List(contentBrowserConverter),
    postProcessorConverters = List(FileDivConverter,
                                   SimpleTagConverter,
                                   TableConverter,
                                   MathMLConverter,
                                   htmlCleaner,
                                   VisualElementConverter,
                                   RelatedContentConverter)
  )

  lazy val conceptConverter = ConverterPipeLine(
    mainConverters = List(contentBrowserConverter),
    postProcessorConverters = List(ConceptConverter)
  )

  lazy val leafNodeConverter = ConverterPipeLine(
    mainConverters = Seq(contentBrowserConverter),
    postProcessorConverters = List(LeafNodeConverter) ++ articleConverter.postProcessorConverters
  )

  private lazy val Converters = Map(
    nodeTypeBegrep -> conceptConverter,
    nodeTypeH5P -> leafNodeConverter,
    nodeTypeVideo -> leafNodeConverter,
    nodeTypeLink -> leafNodeConverter
  ).withDefaultValue(articleConverter)

  def executeConverterModules(nodeToConvert: NodeToConvert,
                              importStatus: ImportStatus): Try[(NodeToConvert, ImportStatus)] =
    runConverters(Converters(nodeToConvert.nodeType.toLowerCase).mainConverters, nodeToConvert, importStatus)

  def executePostprocessorModules(nodeToConvert: NodeToConvert,
                                  importStatus: ImportStatus): Try[(NodeToConvert, ImportStatus)] =
    runConverters(Converters(nodeToConvert.nodeType.toLowerCase).postProcessorConverters, nodeToConvert, importStatus)

  private def runConverters(converters: Seq[ConverterModule],
                            nodeToConvert: NodeToConvert,
                            importStatus: ImportStatus): Try[(NodeToConvert, ImportStatus)] = {
    val (convertedNode, finalImportStatus, exceptions) =
      converters.foldLeft((nodeToConvert, importStatus, Seq[Throwable]()))((element, converter) => {

        val (partiallyConvertedNode, importStatus, exceptions) = element
        converter.convert(partiallyConvertedNode, importStatus) match {
          case Success((updatedNode, updatedImportStatus)) =>
            (updatedNode, updatedImportStatus, exceptions)
          case Failure(x) =>
            (partiallyConvertedNode, importStatus, exceptions :+ x)
        }
      })

    if (exceptions.nonEmpty) {
      val failedNodeIds = nodeToConvert.contents.map(_.nid)
      Failure(ImportExceptions(failedNodeIds.toSet, errors = exceptions))
    } else {
      Success((convertedNode, finalImportStatus))
    }

  }

}
