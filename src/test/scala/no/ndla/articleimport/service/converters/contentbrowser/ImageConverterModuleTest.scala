/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters.contentbrowser

import no.ndla.articleimport.model.domain.{Author, ImportStatus}
import no.ndla.articleimport.integration._
import no.ndla.articleimport.{TestData, TestEnvironment, UnitSuite}
import no.ndla.validation.EmbedTagRules.ResourceHtmlEmbedTag
import org.mockito.Mockito._

import scala.util.Success

class ImageConverterModuleTest extends UnitSuite with TestEnvironment {
  val nodeId = "1234"
  val altText = "Jente som spiser melom. Grønn bakgrunn, rød melon. Fotografi."
  val caption = "sample \"image\" ; <> æøå ~ é õ caption"
  val expectedCaption = "sample &quot;image&quot; ; <> æøå ~ é õ caption"

  val content = TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "alt" -> altText, "link_text" -> caption)
  val license = ImageLicense("licence", "description", Some("http://"))
  val author = Author("forfatter", "Henrik")
  val copyright = ImageCopyright(license, "", List(author))

  val image = ImageMetaInformation("1234",
                                   Some(ImageTitle("", "nb")),
                                   Some(ImageAltText("", "nb")),
                                   "full.jpg",
                                   1024,
                                   "",
                                   copyright,
                                   ImageTag(List(""), ""))

  test("That a contentbrowser string of type 'image' returns an HTML img-tag with path to image") {
    val expectedResult =
      s"""<$ResourceHtmlEmbedTag data-align="" data-alt="$altText" data-caption="$expectedCaption" data-resource="image" data-resource_id="1234" data-size="full" />"""

    when(imageApiClient.importImage(nodeId)).thenReturn(Some(image))

    val Success((result, requiredLibraries, errors)) =
      ImageConverterModule.convert(content, ImportStatus.empty)
    result should equal(expectedResult)
    errors.messages.length should equal(0)
    requiredLibraries.length should equal(0)
  }

  test("That the the data-captions attribute is empty if no captions exist") {
    val contentEmptyCaption =
      TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "alt" -> altText, "link_text" -> "")
    val expectedResult =
      s"""<$ResourceHtmlEmbedTag data-align="" data-alt="$altText" data-caption="" data-resource="image" data-resource_id="1234" data-size="full" />"""

    when(imageApiClient.importImage(nodeId)).thenReturn(Some(image))
    val Success((result, requiredLibraries, errors)) =
      ImageConverterModule.convert(contentEmptyCaption, ImportStatus.empty)

    result should equal(expectedResult)
    errors.messages.length should equal(0)
    requiredLibraries.length should equal(0)
  }

  test("That a contentbrowser string of type 'image' returns a Failure if image is inexistant") {
    val expectedResult =
      s"""<img src='stock.jpeg' alt='The image with id $nodeId was not not found' />"""

    when(imageApiClient.importImage(nodeId)).thenReturn(None)
    ImageConverterModule.convert(content, ImportStatus.empty).isFailure should be(true)
  }

  test("That a the html tag contains an alignment attribute with the correct value") {
    val contentWithLeftMargin =
      TestData.contentBrowserWithFields(List.empty,
                                        "nid" -> nodeId,
                                        "alt" -> altText,
                                        "link_text" -> caption,
                                        "css_class" -> "contentbrowser contentbrowser_margin_left")
    val expectedResult =
      s"""<$ResourceHtmlEmbedTag data-align="right" data-alt="$altText" data-caption="$expectedCaption" data-resource="image" data-resource_id="1234" data-size="full" />"""

    when(imageApiClient.importImage(nodeId)).thenReturn(Some(image))
    val Success((result, requiredLibraries, errors)) =
      ImageConverterModule.convert(contentWithLeftMargin, ImportStatus.empty)

    result should equal(expectedResult)
    errors.messages.length should equal(0)
    requiredLibraries.length should equal(0)
  }

  test("image width width should map to their corresponding sizes") {
    when(imageApiClient.importImage(nodeId)).thenReturn(Some(image))

    val Success((result, _, _)) =
      ImageConverterModule.convert(TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "width" -> "50"),
                                   ImportStatus.empty)
    val expectedResult =
      s"""<$ResourceHtmlEmbedTag data-align="" data-alt="" data-caption="" data-resource="image" data-resource_id="1234" data-size="xsmall" />"""
    result should equal(expectedResult)

    val Success((result2, _, _)) =
      ImageConverterModule.convert(TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "width" -> "100"),
                                   ImportStatus.empty)
    val expectedResult2 =
      s"""<$ResourceHtmlEmbedTag data-align="" data-alt="" data-caption="" data-resource="image" data-resource_id="1234" data-size="small" />"""
    result2 should equal(expectedResult2)

    val Success((result3, _, _)) = ImageConverterModule.convert(
      TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "width" -> "300", "imagecache" -> "Hoyrespalte"),
      ImportStatus.empty)
    val expectedResult3 =
      s"""<$ResourceHtmlEmbedTag data-align="" data-alt="" data-caption="" data-resource="image" data-resource_id="1234" data-size="full" />"""
    result3 should equal(expectedResult3)
  }

  test("image width imagecache should map to their corresponding sizes") {
    when(imageApiClient.importImage(nodeId)).thenReturn(Some(image))

    val Success((result, _, _)) =
      ImageConverterModule.convert(
        TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "imagecache" -> "Liten"),
        ImportStatus.empty)
    val expectedResult =
      s"""<$ResourceHtmlEmbedTag data-align="" data-alt="" data-caption="" data-resource="image" data-resource_id="1234" data-size="xsmall" />"""
    result should equal(expectedResult)

    val Success((result2, _, _)) =
      ImageConverterModule.convert(
        TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "imagecache" -> "Hoyrespalte"),
        ImportStatus.empty)
    val expectedResult2 =
      s"""<$ResourceHtmlEmbedTag data-align="" data-alt="" data-caption="" data-resource="image" data-resource_id="1234" data-size="small" />"""
    result2 should equal(expectedResult2)

    val Success((result3, _, _)) =
      ImageConverterModule.convert(
        TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "imagecache" -> "Fullbredde"),
        ImportStatus.empty)
    val expectedResult3 =
      s"""<$ResourceHtmlEmbedTag data-align="" data-alt="" data-caption="" data-resource="image" data-resource_id="1234" data-size="full" />"""
    result3 should equal(expectedResult3)
  }
}
