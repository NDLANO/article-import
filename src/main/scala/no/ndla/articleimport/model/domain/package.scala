/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */
package no.ndla.articleimport.model

package object domain {

  def emptySomeToNone(lang: Option[String]): Option[String] = lang.filter(_.nonEmpty)

  def emptyStringToNone(str: String): Option[String] = emptySomeToNone(Option(str.trim))

  case class ArticleIds(articleId: Long, externalId: List[String], importId: Option[String])
  case class ImportId(importId: Option[String])
}
