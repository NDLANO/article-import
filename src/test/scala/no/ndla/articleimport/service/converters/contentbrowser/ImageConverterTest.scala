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

class ImageConverterTest extends UnitSuite with TestEnvironment {
  val nodeId = "1234"
  val altText = "Jente som spiser melom. Grønn bakgrunn, rød melon. Fotografi."
  val caption = "sample \"image\" ; <> æøå ~ é õ caption"
  val expectedCaption = "sample &quot;image&quot; ; <> æøå ~ é õ caption"

  val contentString = s"[contentbrowser ==nid=$nodeId==imagecache=Fullbredde==width===alt=$altText==link===node_link=1==link_type=link_to_content==lightbox_size===remove_fields[76661]=1==remove_fields[76663]=1==remove_fields[76664]=1==remove_fields[76666]=1==insertion===link_title_text= ==link_text=$caption==text_align===css_class=contentbrowser contentbrowser]"
  val contentStringWithLeftMargin = s"[contentbrowser ==nid=$nodeId==imagecache=Fullbredde==width===alt=$altText==link===node_link=1==link_type=link_to_content==lightbox_size===remove_fields[76661]=1==remove_fields[76663]=1==remove_fields[76664]=1==remove_fields[76666]=1==insertion===link_title_text= ==link_text=$caption==text_align===css_class=contentbrowser contentbrowser_margin_left contentbrowser]"
  val contentStringEmptyCaption = s"[contentbrowser ==nid=$nodeId==imagecache=Fullbredde==width===alt=$altText==link===node_link=1==link_type=link_to_content==lightbox_size===remove_fields[76661]=1==remove_fields[76663]=1==remove_fields[76664]=1==remove_fields[76666]=1==insertion===link_title_text= ==link_text===text_align===css_class=contentbrowser contentbrowser]"
  val content = ContentBrowserString(contentString, "nb")
  val license = ImageLicense("licence", "description", Some("http://"))
  val author = Author("forfatter", "Henrik")
  val copyright = ImageCopyright(license, "", List(author))

  val image = ImageMetaInformation("1234", List(ImageTitle("", Some("nb"))), List(ImageAltText("", Some("nb"))), "full.jpg", 1024, "", copyright, ImageTag(List(""), Some("")))

  test("That a contentbrowser string of type 'image' returns an HTML img-tag with path to image") {
    val expectedResult = s"""<$ResourceHtmlEmbedTag data-align="" data-alt="$altText" data-caption="$expectedCaption" data-resource="image" data-resource_id="1234" data-size="full" />"""

    when(imageApiClient.importImage(nodeId)).thenReturn(Some(image))

    val Success((result, requiredLibraries, errors)) = ImageConverter.convert(content, ImportStatus.empty)
    result should equal (expectedResult)
    errors.messages.length should equal(0)
    requiredLibraries.length should equal(0)
  }

  test("That the the data-captions attribute is empty if no captions exist") {
    val expectedResult = s"""<$ResourceHtmlEmbedTag data-align="" data-alt="$altText" data-caption="" data-resource="image" data-resource_id="1234" data-size="full" />"""

    when(imageApiClient.importImage(nodeId)).thenReturn(Some(image))
    val Success((result, requiredLibraries, errors)) = ImageConverter.convert(ContentBrowserString(contentStringEmptyCaption, "nb"), ImportStatus.empty)

    result should equal (expectedResult)
    errors.messages.length should equal(0)
    requiredLibraries.length should equal(0)
  }

  test("That a contentbrowser string of type 'image' returns a Failure if image is inexistant") {
    val expectedResult = s"""<img src='stock.jpeg' alt='The image with id $nodeId was not not found' />"""

    when(imageApiClient.importImage(nodeId)).thenReturn(None)
    ImageConverter.convert(content, ImportStatus.empty).isFailure should be (true)
  }

  test("That a the html tag contains an alignment attribute with the correct value") {
    val expectedResult = s"""<$ResourceHtmlEmbedTag data-align="right" data-alt="$altText" data-caption="$expectedCaption" data-resource="image" data-resource_id="1234" data-size="full" />"""

    when(imageApiClient.importImage(nodeId)).thenReturn(Some(image))
    val Success((result, requiredLibraries, errors)) = ImageConverter.convert(ContentBrowserString(contentStringWithLeftMargin, "nb"), ImportStatus.empty)

    result should equal (expectedResult)
    errors.messages.length should equal(0)
    requiredLibraries.length should equal(0)
  }

  test("image width width should map to their corresponding sizes") {
    when(imageApiClient.importImage(nodeId)).thenReturn(Some(image))

    val Success((result, _, _)) = ImageConverter.convert(TestData.contentBrowserWithFields("nid" -> nodeId, "width" -> "50"), ImportStatus.empty)
    val expectedResult = s"""<$ResourceHtmlEmbedTag data-align="" data-alt="" data-caption="" data-resource="image" data-resource_id="1234" data-size="xsmall" />"""
    result should equal (expectedResult)

    val Success((result2, _, _)) = ImageConverter.convert(TestData.contentBrowserWithFields("nid" -> nodeId, "width" -> "100"), ImportStatus.empty)
    val expectedResult2 = s"""<$ResourceHtmlEmbedTag data-align="" data-alt="" data-caption="" data-resource="image" data-resource_id="1234" data-size="small" />"""
    result2 should equal (expectedResult2)

    val Success((result3, _, _)) = ImageConverter.convert(TestData.contentBrowserWithFields("nid" -> nodeId, "width" -> "300", "imagecache" -> "Hoyrespalte"), ImportStatus.empty)
    val expectedResult3 = s"""<$ResourceHtmlEmbedTag data-align="" data-alt="" data-caption="" data-resource="image" data-resource_id="1234" data-size="full" />"""
    result3 should equal (expectedResult3)
  }

  test("image width imagecache should map to their corresponding sizes") {
    when(imageApiClient.importImage(nodeId)).thenReturn(Some(image))

    val Success((result, _, _)) = ImageConverter.convert(TestData.contentBrowserWithFields("nid" -> nodeId, "imagecache" -> "Liten"), ImportStatus.empty)
    val expectedResult = s"""<$ResourceHtmlEmbedTag data-align="" data-alt="" data-caption="" data-resource="image" data-resource_id="1234" data-size="xsmall" />"""
    result should equal (expectedResult)

    val Success((result2, _, _)) = ImageConverter.convert(TestData.contentBrowserWithFields("nid" -> nodeId, "imagecache" -> "Hoyrespalte"), ImportStatus.empty)
    val expectedResult2 = s"""<$ResourceHtmlEmbedTag data-align="" data-alt="" data-caption="" data-resource="image" data-resource_id="1234" data-size="small" />"""
    result2 should equal (expectedResult2)

    val Success((result3, _, _)) = ImageConverter.convert(TestData.contentBrowserWithFields("nid" -> nodeId, "imagecache" -> "Fullbredde"), ImportStatus.empty)
    val expectedResult3 = s"""<$ResourceHtmlEmbedTag data-align="" data-alt="" data-caption="" data-resource="image" data-resource_id="1234" data-size="full" />"""
    result3 should equal (expectedResult3)
  }
}
