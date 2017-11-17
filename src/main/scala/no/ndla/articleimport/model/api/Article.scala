/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.model.api

import java.util.Date

import org.scalatra.swagger.annotations.{ApiModel, ApiModelProperty}

import scala.annotation.meta.field

@ApiModel(description = "Information about the article")
case class Article(@(ApiModelProperty@field)(description = "The unique id of the article") id: Long,
                   @(ApiModelProperty@field)(description = "Link to article on old platform") oldNdlaUrl: Option[String],
                   @(ApiModelProperty@field)(description = "The revision number for the article") revision: Int,
                   @(ApiModelProperty@field)(description = "Available titles for the article") title: ArticleTitle,
                   @(ApiModelProperty@field)(description = "The content of the article in available languages") content: ArticleContent,
                   @(ApiModelProperty@field)(description = "Describes the copyright information for the article") copyright: Copyright,
                   @(ApiModelProperty@field)(description = "Searchable tags for the article") tags: ArticleTag,
                   @(ApiModelProperty@field)(description = "Required libraries in order to render the article") requiredLibraries: Seq[RequiredLibrary],
                   @(ApiModelProperty@field)(description = "A visual element article") visualElement: Option[VisualElement],
                   @(ApiModelProperty@field)(description = "A meta image for the article") metaImage: Option[String],
                   @(ApiModelProperty@field)(description = "An introduction for the article") introduction: Option[ArticleIntroduction],
                   @(ApiModelProperty@field)(description = "Meta description for the article") metaDescription: ArticleMetaDescription,
                   @(ApiModelProperty@field)(description = "When the article was created") created: Date,
                   @(ApiModelProperty@field)(description = "When the article was last updated") updated: Date,
                   @(ApiModelProperty@field)(description = "By whom the article was last updated") updatedBy: String,
                   @(ApiModelProperty@field)(description = "The type of article this is. Possible values are topic-article,standard") articleType: String,
                   @(ApiModelProperty@field)(description = "The languages this article supports") supportedLanguages: Seq[String]
                    )
