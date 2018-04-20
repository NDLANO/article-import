/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */
package no.ndla.articleimport.model

package object domain {

  def emptySomeToNone(lang: Option[String]): Option[String] = {
    lang.filter(_.nonEmpty)
  }

  case class ArticleIds(articleId: Long, externalId: Option[String])

  def getByLanguage[T](entries: Seq[LanguageField[T]], language: String): Option[T] =
    entries.find(_.language == language).map(_.value)
}
