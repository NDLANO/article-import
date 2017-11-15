/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.model.domain

trait WithLanguage {
  def language: String
}

trait LanguageField[T] extends WithLanguage {
  def value: T
  def language: String
}
