/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters.contentbrowser

import no.ndla.articleimport.{TestData, TestEnvironment, UnitSuite}
import no.ndla.validation.EmbedTagRules.ResourceHtmlEmbedTag
import no.ndla.articleimport.model.domain.ImportStatus
import org.mockito.Mockito._

import scala.util.Success

class H5PConverterTest extends UnitSuite with TestEnvironment {
  val nodeId = "1234"
  val altText = "Jente som spiser melom. Grønn bakgrunn, rød melon. Fotografi."

  val content = TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "alt" -> altText)

  test("That contentbrowser strings of type 'h5p_content' returns an iframe") {
    when(h5pApiClient.getViewFromOldId("1234"))
      .thenReturn(Some(s"//ndla.no/h5p/embed/1234"))
    val expectedResult =
      s"""<$ResourceHtmlEmbedTag data-resource="external" data-url="//ndla.no/h5p/embed/1234" />"""
    val Success((result, requiredLibraries, errors)) =
      H5PConverter.convert(content, ImportStatus.empty)

    result should equal(expectedResult)
    errors.messages.length should equal(0)
    requiredLibraries.length should equal(0)
  }
}
