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

trait VeiledningConverterModule extends GeneralContentConverterModule {
  this: ExtractService with ExtractConvertStoreContent with DraftApiClient with HtmlTagGenerator =>

  object VeiledningConverterModule extends GeneralContentConverterModule {
    override val typeName: String = "veiledning"
  }
}
