/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters.contentbrowser

import no.ndla.articleimport.integration.DraftApiClient
import no.ndla.articleimport.service.converters.HtmlTagGenerator
import no.ndla.articleimport.service.{ConverterService, ExtractConvertStoreContent, ExtractService}

trait OppgaveConverterModule extends GeneralContentConverterModule {
  this: ExtractService
    with ExtractConvertStoreContent
    with ConverterService
    with DraftApiClient
    with HtmlTagGenerator =>

  object OppgaveConverter extends GeneralContentConverter {
    override val typeName: String = "oppgave"
  }
}
