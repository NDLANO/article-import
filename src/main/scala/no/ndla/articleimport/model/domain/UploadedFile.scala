/*
 * Part of NDLA article_import.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.model.domain

import no.ndla.articleimport.ArticleImportProperties.Domain

case class UploadedFile(fileMeta: ContentFilMeta, filePath: String) {
  val url: String = s"$Domain/files/$filePath"
}
