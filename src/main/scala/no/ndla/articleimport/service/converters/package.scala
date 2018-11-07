/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service
import scala.util.matching.Regex

package object converters {
  val NBSP = "&#xa0;" // standard notation for &nbsp; used by jsoup
  val LightboxPattern: Regex = "(lightbox_.*)".r // Regex for detecting lightbox insertion mode in contentBrowsers
}
