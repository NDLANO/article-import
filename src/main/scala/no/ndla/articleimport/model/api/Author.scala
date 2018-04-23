/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.model.api

import org.scalatra.swagger.annotations.{ApiModel, ApiModelProperty}
import scala.annotation.meta.field

@ApiModel(description = "Information about an author")
case class Author(@(ApiModelProperty @field)(
                    description = "The description of the author. Eg. Photographer or Supplier") `type`: String,
                  @(ApiModelProperty @field)(description = "The name of the of the author") name: String)
