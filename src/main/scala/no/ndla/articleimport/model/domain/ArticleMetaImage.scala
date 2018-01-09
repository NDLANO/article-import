/*
 * Part of NDLA article_import.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.model.domain

case class ArticleMetaImage(imageId: String, language: String) extends LanguageField[String] {
  override def value: String = imageId
}
