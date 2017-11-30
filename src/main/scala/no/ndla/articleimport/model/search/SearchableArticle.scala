/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.model.search

import java.util.Date

import no.ndla.articleimport.model.domain.emptySomeToNone
import no.ndla.articleimport.model.search.LanguageValue.{LanguageValue => LV}


object LanguageValue {

  case class LanguageValue[T](lang: String, value: T)

  def apply[T](lang: String, value: T): LanguageValue[T] = LanguageValue(lang, value)

}

case class SearchableLanguageValues(languageValues: Seq[LV[String]])

case class SearchableLanguageList(languageValues: Seq[LV[Seq[String]]])

case class SearchableArticle(
  id: Long,
  title: SearchableLanguageValues,
  content: SearchableLanguageValues,
  visualElement: SearchableLanguageValues,
  introduction: SearchableLanguageValues,
  tags: SearchableLanguageList,
  lastUpdated: Date,
  license: String,
  authors: Seq[String],
  articleType: String
)

case class SearchableConcept(
  id: Long,
  title: SearchableLanguageValues,
  content: SearchableLanguageValues
)