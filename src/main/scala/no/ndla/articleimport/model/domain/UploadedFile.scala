/*
 * Part of NDLA article_import.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.model.domain

case class UploadedFile(fileMeta: ContentFilMeta, filePath: String) {
  val urlPath: String = s"files/$filePath"
}
