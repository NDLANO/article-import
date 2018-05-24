/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters.contentbrowser

import no.ndla.articleimport.integration.DraftApiClient
import no.ndla.articleimport.service.converters.HtmlTagGenerator
import no.ndla.articleimport.service.{ExtractConvertStoreContent, ExtractService}

trait AktualitetConverterModule extends GeneralContentConverterModule {
  this: ExtractService with ExtractConvertStoreContent with HtmlTagGenerator with DraftApiClient =>

  object AktualitetConverterModule extends GeneralContentConverterModule {
    override val typeName: String = "aktualitet"
  }
}
