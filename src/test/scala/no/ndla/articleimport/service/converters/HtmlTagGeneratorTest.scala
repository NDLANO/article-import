/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters

import no.ndla.articleimport.{TestEnvironment, UnitSuite}
import no.ndla.validation.EmbedTagRules.ResourceHtmlEmbedTag
import no.ndla.validation.{Attributes, ResourceType}

class HtmlTagGeneratorTest extends UnitSuite with TestEnvironment {
    val sampleDataAttributes = Map(
      Attributes.DataResource -> ResourceType.Image.toString,
      Attributes.DataUrl -> "http://localhost/1",
      Attributes.DataCaption -> "Sample image"
    )

    val sampleContentLink = Map(
      Attributes.DataResource -> ResourceType.ContentLink.toString,
      Attributes.DataContentId -> "2",
      Attributes.DataLinkText -> "http://localhost/2",
      Attributes.DataOpenIn -> "new-context"
    )

  test("A correctly formatted figure tag is returned") {
    val figureString: String = HtmlTagGenerator.buildEmbedContent(sampleDataAttributes)
    val expected = s"""<$ResourceHtmlEmbedTag data-caption="Sample image" data-resource="image" data-url="http://localhost/1" />"""

    figureString should equal(expected)
  }

  test("Correctly formatted content-link embed") {
    val contentLinkString: String = HtmlTagGenerator.buildEmbedContent(sampleContentLink)
    val expected = s"""<$ResourceHtmlEmbedTag data-content-id="2" data-link-text="http://localhost/2" data-open-in="new-context" data-resource="content-link" />"""

    contentLinkString should equal(expected)
  }

}
