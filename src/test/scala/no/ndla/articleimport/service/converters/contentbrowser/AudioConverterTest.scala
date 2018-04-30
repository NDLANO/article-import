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

import scala.util.{Failure, Success}

class AudioConverterTest extends UnitSuite with TestEnvironment {
  val nodeId = "1234"
  val altText = "Jente som spiser melom. Grønn bakgrunn, rød melon. Fotografi."
  val caption = "This is caption"

  val content = TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "alt" -> altText, "link_text" -> caption)

  test("That AudioConverter returns a embed resource string if the audio was imported") {
    val audioId: Long = 123
    val expectedResult =
      s"""<$ResourceHtmlEmbedTag data-caption="$caption" data-resource="audio" data-resource_id="123" data-type="standard" />"""

    when(audioApiClient.getOrImportAudio(nodeId)).thenReturn(Success(audioId))

    val Success((result, requiredLibraries, status)) =
      AudioConverter.convert(content, ImportStatus.empty)
    val strippedResult = " +".r.replaceAllIn(result.replace("\n", ""), " ")

    status.messages.isEmpty should be(true)
    requiredLibraries.isEmpty should be(true)
    strippedResult should equal(expectedResult)
  }

  test("data-type should be minimal if is inside html table") {
    val audioId: Long = 123
    val expectedResult =
      s"""<$ResourceHtmlEmbedTag data-caption="$caption" data-resource="audio" data-resource_id="123" data-type="minimal" />"""

    val contentInsideTable = TestData.contentBrowserWithFields(List("td", "tr", "tbody", "table", "div", "section"),
                                                               "nid" -> nodeId,
                                                               "alt" -> altText,
                                                               "link_text" -> caption)
    when(audioApiClient.getOrImportAudio(nodeId)).thenReturn(Success(audioId))

    val Success((result, requiredLibraries, status)) =
      AudioConverter.convert(contentInsideTable, ImportStatus.empty)
    val strippedResult = " +".r.replaceAllIn(result.replace("\n", ""), " ")

    status.messages.isEmpty should be(true)
    requiredLibraries.isEmpty should be(true)
    strippedResult should equal(expectedResult)
  }

  test("That AudioConverter returns a Failure if the audio was not found") {
    when(audioApiClient.getOrImportAudio(nodeId))
      .thenReturn(Failure(new RuntimeException("error")))
    AudioConverter.convert(content, ImportStatus.empty).isFailure should be(true)
  }
}
