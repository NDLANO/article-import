/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.model.api

import org.scalatra.swagger.annotations.{ApiModel, ApiModelProperty}
import scala.annotation.meta.field

@ApiModel(description = "Description of copyright information")
case class Copyright(@(ApiModelProperty@field)(description = "Describes the license of the article") license: License,
                     @(ApiModelProperty@field)(description = "Reference to where the article is procured") origin: String,
                     @(ApiModelProperty@field)(description = "List of authors") authors: Seq[Author])
