/*
 * Part of NDLA article_import.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service

import scala.util.matching.Regex

object DomainRegex {
  // matches on a domain and all subdomains
  def domainRegex(host: String): Regex = raw"""^([A-Za-z0-9]+\.)*$host$$""".r

  implicit class DomainRegex(host: String) {
    def asDomainRegex: Regex = domainRegex(host)
  }
}
