/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.articleimport

import com.amazonaws.services.s3.AmazonS3Client
import com.typesafe.scalalogging.LazyLogging
import no.ndla.articleimport.auth.{Role, User}
import no.ndla.articleimport.controller._
import no.ndla.articleimport.integration._
import no.ndla.articleimport.service._
import no.ndla.articleimport.service.converters._
import no.ndla.articleimport.service.converters.contentbrowser._
import no.ndla.network.NdlaClient
import org.scalatest.mockito.MockitoSugar

trait TestEnvironment
  extends LazyLogging
    with InternController
    with HealthController
    with MockitoSugar
    with ExtractService
    with ConverterModules
    with ConverterService
    with LeafNodeConverter
    with ContentBrowserConverterModules
    with ContentBrowserConverter
    with BiblioConverterModule
    with VisualElementConverter
    with RelatedContentConverter
    with AmazonClient
    with AttachmentStorageService
    with ExtractConvertStoreContent
    with NdlaClient
    with DraftApiClient
    with MigrationApiClient
    with AudioApiClient
    with ImageApiClient
    with TagsService
    with HtmlTagGenerator
    with HTMLCleaner
    with Clock
    with User
    with Role {

  val internController = mock[InternController]

  val healthController = mock[HealthController]

  val amazonClient = mock[AmazonS3Client]
  val attachmentStorageName = "testStorageName"

  val extractConvertStoreContent = mock[ExtractConvertStoreContent]

  val extractService = mock[ExtractService]

  val converterService = mock[ConverterService]
  val contentBrowserConverter = new ContentBrowserConverter
  val htmlCleaner = new HTMLCleaner

  lazy val articleConverter = ConverterPipeLine(
    mainConverters = List(contentBrowserConverter),
    postProcessorConverters = List(SimpleTagConverter, TableConverter, MathMLConverter, RelatedContentConverter, htmlCleaner, VisualElementConverter)
  )
  lazy val conceptConverter = ConverterPipeLine(
    mainConverters = List(contentBrowserConverter),
    postProcessorConverters = List(ConceptConverter)
  )
  override lazy val leafNodeConverter = ConverterPipeLine(
    mainConverters = Seq(contentBrowserConverter),
    postProcessorConverters = List(LeafNodeConverter) ++ articleConverter.postProcessorConverters
  )
  val attachmentStorageService = mock[AmazonStorageService]

  val tagsService = mock[TagsService]
  val ndlaClient = mock[NdlaClient]
  val migrationApiClient = mock[MigrationApiClient]
  val audioApiClient = mock[AudioApiClient]
  val imageApiClient = mock[ImageApiClient]
  val draftApiClient = mock[DraftApiClient]
  val h5pApiClient = mock[H5PApiClient]

  val clock = mock[SystemClock]
  val authUser = mock[AuthUser]
  val authRole = new AuthRole

}
