/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.model.api

case class UpdateArticle(revision: Int,
                         language: String,
                         title: Option[String],
                         content: Option[String],
                         tags: Seq[String],
                         introduction: Option[String],
                         metaDescription: Option[String],
                         metaImage: Option[NewArticleMetaImage],
                         visualElement: Option[String],
                         copyright: Option[Copyright],
                         requiredLibraries: Seq[RequiredLibrary],
                         notes: Seq[String],
                         articleType: Option[String])
