/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters.contentbrowser

import no.ndla.articleimport.ArticleImportProperties.{NDLABrightcoveAccountId, NDLABrightcovePlayerId}
import no.ndla.validation.EmbedTagRules.ResourceHtmlEmbedTag
import no.ndla.articleimport.model.domain.ImportStatus
import no.ndla.articleimport.{TestData, TestEnvironment, UnitSuite}
import no.ndla.validation.ResourceType
import org.mockito.Mockito._

import scala.util.Success

class VideoConverterModuleTest extends UnitSuite with TestEnvironment {
  val nodeId = "1234"
  val altText = "Jente som spiser melom. Grønn bakgrunn, rød melon. Fotografi."
  val caption = "sample caption"

  test("That VideoConverter converts a ContentBrowser to html code") {
    val content = TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "alt" -> altText)
    val expectedResult =
      s"""<$ResourceHtmlEmbedTag data-account="$NDLABrightcoveAccountId" data-caption="" data-player="$NDLABrightcovePlayerId" data-resource="brightcove" data-videoid="ref:$nodeId" />"""
    val Success((result, requiredLibraries, _)) =
      VideoConverterModule.convert(content, ImportStatus.empty)

    result should equal(expectedResult)
    requiredLibraries.length should equal(0)
  }

  test("Captions are added as video metadata") {
    val contentWithCaptions =
      TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "alt" -> altText, "link_text" -> caption)
    val expectedResult =
      s"""<$ResourceHtmlEmbedTag data-account="$NDLABrightcoveAccountId" data-caption="$caption" data-player="$NDLABrightcovePlayerId" data-resource="brightcove" data-videoid="ref:$nodeId" />"""
    val Success((result, requiredLibraries, _)) =
      VideoConverterModule.convert(contentWithCaptions, ImportStatus.empty)

    result should equal(expectedResult)
    requiredLibraries.length should equal(0)
  }

  test("the contentbrowser should be converted to a link if insertion method is link") {
    val contentWithInsertionLink = TestData.contentBrowserWithFields(List.empty,
                                                                     "nid" -> nodeId,
                                                                     "alt" -> altText,
                                                                     "link_text" -> caption,
                                                                     "insertion" -> "link")
    val expectedResult =
      s"""<$ResourceHtmlEmbedTag data-content-id="1" data-link-text="$caption" data-resource="${ResourceType.ContentLink}" />"""

    when(extractConvertStoreContent.processNode(nodeId, ImportStatus.empty))
      .thenReturn(Success(TestData.sampleApiArticle, ImportStatus.empty))

    val Success((result, requiredLibraries, _)) =
      VideoConverterModule.convert(contentWithInsertionLink, ImportStatus.empty)
    result should equal(expectedResult)
    requiredLibraries.length should equal(0)
  }
}
