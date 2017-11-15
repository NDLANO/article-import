/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.model.domain

case class ConceptContent(content: String, language: String) extends LanguageField[String] {
  override def value: String = content
}
