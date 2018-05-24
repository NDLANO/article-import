/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters.contentbrowser

import com.typesafe.scalalogging.LazyLogging
import no.ndla.articleimport.auth.User
import no.ndla.articleimport.integration._
import no.ndla.articleimport.model.domain.{ImportStatus, RequiredLibrary}
import no.ndla.articleimport.service._
import no.ndla.articleimport.service.converters.{HtmlTagGenerator, MetaInfoConverter}
import no.ndla.network.NdlaClient

import scala.util.Try

trait ContentBrowserConverterModule {
  def convert(content: ContentBrowser, importStatus: ImportStatus): Try[(String, Seq[RequiredLibrary], ImportStatus)]
  val typeName: String
}

trait ContentBrowserConverterModules
    extends ExtractService
    with AttachmentStorageService
    with AmazonClient
    with ConverterModules
    with ConverterService
    with MetaInfoConverter
    with Clock
    with ExtractConvertStoreContent
    with LazyLogging
    with ImageConverterModule
    with ImageApiClient
    with LenkeConverterModule
    with H5PConverterModule
    with OppgaveConverterModule
    with FagstoffConverterModule
    with NonExistentNodeConverterModule
    with AudioConverterModule
    with AudioApiClient
    with H5PApiClient
    with AktualitetConverterModule
    with VideoConverterModule
    with FilConverterModule
    with VeiledningConverterModule
    with BiblioConverterModule
    with BegrepConverterModule
    with TagsService
    with NdlaClient
    with MigrationApiClient
    with DraftApiClient
    with HtmlTagGenerator
    with UnsupportedContentConverterModule
    with User
