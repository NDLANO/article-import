/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.model.domain

import java.util.Date

import no.ndla.validation.{ValidationException, ValidationMessage}

sealed trait Content {
  def id: Option[Long]
}

case class Article(id: Option[Long],
                   revision: Option[Int],
                   title: Seq[ArticleTitle],
                   content: Seq[ArticleContent],
                   copyright: Copyright,
                   tags: Seq[ArticleTag],
                   requiredLibraries: Seq[RequiredLibrary],
                   visualElement: Seq[VisualElement],
                   introduction: Seq[ArticleIntroduction],
                   metaDescription: Seq[ArticleMetaDescription],
                   metaImageId: Option[String],
                   created: Date,
                   updated: Date,
                   updatedBy: String,
                   articleType: String) extends Content


object ArticleType extends Enumeration {
  val Standard = Value("standard")
  val TopicArticle = Value("topic-article")

  def all = ArticleType.values.map(_.toString).toSeq
  def valueOf(s:String): Option[ArticleType.Value] = ArticleType.values.find(_.toString == s)
  def valueOfOrError(s: String): ArticleType.Value =
    valueOf(s).getOrElse(throw new ValidationException(errors = List(ValidationMessage("articleType", s"'$s' is not a valid article type. Valid options are ${all.mkString(",")}."))))
}

case class Concept(id: Option[Long],
                   title: Seq[ConceptTitle],
                   content: Seq[ConceptContent],
                   copyright: Option[Copyright],
                   created: Date,
                   updated: Date) extends Content {
  lazy val supportedLanguages: Set[String] = (content union title).map(_.language).toSet
}

