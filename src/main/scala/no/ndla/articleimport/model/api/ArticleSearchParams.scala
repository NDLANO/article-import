/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.model.api

import org.scalatra.swagger.annotations.{ApiModel, ApiModelProperty}
import scala.annotation.meta.field

@ApiModel(description = "The search parameters")
case class ArticleSearchParams(
    @(ApiModelProperty @field)(description = "The search query") query: Option[String],
    @(ApiModelProperty @field)(description = "The ISO 639-1 language code describing language used in query-params") language: Option[
      String],
    @(ApiModelProperty @field)(description = "Return only articles with provided license.") license: Option[String],
    @(ApiModelProperty @field)(description = "The page number of the search hits to display.") page: Option[Int],
    @(ApiModelProperty @field)(description = "The number of search hits to display for each page.") pageSize: Option[
      Int],
    @(ApiModelProperty @field)(description = "Return only articles that have one of the provided ids") idList: List[
      Long],
    @(ApiModelProperty @field)(description = "Return only articles of specific type(s)") articleTypes: List[String],
    @(ApiModelProperty @field)(description = "The sorting used on results. Default is by -relevance.") sort: Option[
      String])
