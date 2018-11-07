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
import no.ndla.validation.ResourceType
import org.mockito.Mockito._
import org.mockito.ArgumentMatchers.{eq => eqTo, _}

import scala.util.Success

class H5PConverterModuleTest extends UnitSuite with TestEnvironment {
  val nodeId = "1234"
  val altText = "Jente som spiser melom. Grønn bakgrunn, rød melon. Fotografi."

  val content = TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "alt" -> altText)

  test("That contentbrowser strings of type 'h5p_content' returns an iframe") {
    when(h5pApiClient.getViewFromOldId("1234"))
      .thenReturn(Some(s"//ndla.no/h5p/embed/1234"))
    val expectedResult =
      s"""<$ResourceHtmlEmbedTag data-resource="external" data-url="//ndla.no/h5p/embed/1234" />"""
    val Success((result, requiredLibraries, errors)) =
      H5PConverterModule.convert(content, ImportStatus.empty)

    result should equal(expectedResult)
    errors.messages.length should equal(0)
    requiredLibraries.length should equal(0)
  }

  test("That contentbrowser strings of type 'h5p_content', and insertion 'link' is kept as links") {
    reset(extractConvertStoreContent)
    val newArticleId = 100
    val articleWithStatus =
      (TestData.sampleApiArticle.copy(id = newArticleId), ImportStatus.empty.copy(articleId = Some(newArticleId)))
    when(h5pApiClient.getViewFromOldId(nodeId)).thenReturn(Some(s"//ndla.no/h5p/embed/1234"))
    when(extractConvertStoreContent.processNode(eqTo(nodeId), any[ImportStatus])).thenReturn(Success(articleWithStatus))

    val linkText = "Denne urlen leder til en h5p!"

    val toConvert =
      TestData.contentBrowserWithFields(List.empty,
                                        "nid" -> nodeId,
                                        "alt" -> "",
                                        "link_text" -> linkText,
                                        "insertion" -> "link")

    val expectedResult =
      s"""<$ResourceHtmlEmbedTag data-content-id="$newArticleId" data-link-text="$linkText" data-resource="${ResourceType.ContentLink}" />"""
    val Success((result, requiredLibraries, errors)) =
      H5PConverterModule.convert(toConvert, ImportStatus.empty)

    verify(extractConvertStoreContent, times(1)).processNode(eqTo(nodeId), any[ImportStatus])

    result should equal(expectedResult)
    errors.messages.length should equal(0)
    requiredLibraries.length should equal(0)
  }

  test("That contentbrowser strings of type 'h5p_content', and insertion 'lightbox' is kept as links") {
    reset(extractConvertStoreContent)
    val newArticleId = 100
    val articleWithStatus =
      (TestData.sampleApiArticle.copy(id = newArticleId), ImportStatus.empty.copy(articleId = Some(newArticleId)))
    when(h5pApiClient.getViewFromOldId(nodeId)).thenReturn(Some(s"//ndla.no/h5p/embed/1234"))
    when(extractConvertStoreContent.processNode(eqTo(nodeId), any[ImportStatus])).thenReturn(Success(articleWithStatus))

    val linkText = "Denne urlen leder til en h5p!"

    val toConvert =
      TestData.contentBrowserWithFields(List.empty,
                                        "nid" -> nodeId,
                                        "alt" -> "",
                                        "link_text" -> linkText,
                                        "insertion" -> "lightbox_large")

    val expectedResult =
      s"""<$ResourceHtmlEmbedTag data-content-id="$newArticleId" data-link-text="$linkText" data-resource="${ResourceType.ContentLink}" />"""
    val Success((result, requiredLibraries, errors)) =
      H5PConverterModule.convert(toConvert, ImportStatus.empty)

    verify(extractConvertStoreContent, times(1)).processNode(eqTo(nodeId), any[ImportStatus])

    result should equal(expectedResult)
    errors.messages.length should equal(0)
    requiredLibraries.length should equal(0)
  }
}
