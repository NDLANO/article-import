/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport

import com.amazonaws.regions.Regions
import com.amazonaws.services.s3.AmazonS3ClientBuilder
import com.typesafe.scalalogging.LazyLogging
import no.ndla.articleimport.auth.{Role, User}
import no.ndla.articleimport.controller.{HealthController, InternController, NdlaController}
import no.ndla.articleimport.integration._
import no.ndla.articleimport.service._
import no.ndla.articleimport.service.converters._
import no.ndla.articleimport.service.converters.contentbrowser._
import no.ndla.network.NdlaClient

object ComponentRegistry
    extends InternController
    with HealthController
    with NdlaController
    with LazyLogging
    with ExtractService
    with ConverterModules
    with ConverterService
    with MetaInfoConverter
    with LeafNodeConverter
    with ContentBrowserConverterModules
    with ContentBrowserConverter
    with BiblioConverterModule
    with VisualElementConverter
    with AmazonClient
    with AttachmentStorageService
    with ExtractConvertStoreContent
    with NdlaClient
    with MigrationApiClient
    with DraftApiClient
    with AudioApiClient
    with ImageApiClient
    with TagsService
    with HTMLCleaner
    with RelatedContentConverter
    with HtmlTagGenerator
    with Clock
    with Role
    with User {

  lazy val extractConvertStoreContent = new ExtractConvertStoreContent
  lazy val internController = new InternController
  lazy val healthController = new HealthController

  val amazonClient =
    AmazonS3ClientBuilder.standard().withRegion(Regions.EU_CENTRAL_1).build()
  lazy val attachmentStorageName = ArticleImportProperties.AttachmentStorageName
  lazy val attachmentStorageService = new AmazonStorageService

  lazy val migrationApiClient = new MigrationApiClient
  lazy val ndlaClient = new NdlaClient
  lazy val audioApiClient = new AudioApiClient
  lazy val imageApiClient = new ImageApiClient
  lazy val draftApiClient = new DraftApiClient
  lazy val h5pApiClient = new H5PApiClient

  lazy val extractService = new ExtractService
  lazy val converterService = new ConverterService

  lazy val tagsService = new TagsService
  lazy val contentBrowserConverter = new ContentBrowserConverter
  lazy val htmlCleaner = new HTMLCleaner

  override lazy val articleConverter = ConverterPipeLine(
    mainConverters = List(contentBrowserConverter),
    postProcessorConverters = List(SimpleTagConverter,
                                   TableConverter,
                                   MathMLConverter,
                                   htmlCleaner,
                                   VisualElementConverter,
                                   RelatedContentConverter)
  )
  override lazy val conceptConverter = ConverterPipeLine(
    mainConverters = List(contentBrowserConverter),
    postProcessorConverters = List(ConceptConverter)
  )
  override lazy val leafNodeConverter = ConverterPipeLine(
    mainConverters = Seq(contentBrowserConverter),
    postProcessorConverters = List(LeafNodeConverter) ++ articleConverter.postProcessorConverters
  )

  lazy val clock = new SystemClock
  lazy val authRole = new AuthRole
  lazy val authUser = new AuthUser
}
