/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.model.api

case class NewArticle(title: String,
                      content: String,
                      tags: Seq[String],
                      introduction: Option[String],
                      metaDescription: Option[String],
                      metaImageId: Option[String],
                      visualElement: Option[String],
                      copyright: Copyright,
                      requiredLibraries: Seq[RequiredLibrary],
                      articleType: String,
                      notes: Seq[String],
                      language: String)

