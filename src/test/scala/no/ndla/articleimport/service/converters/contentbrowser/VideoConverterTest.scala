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

class VideoConverterTest extends UnitSuite with TestEnvironment {
  val nodeId = "1234"
  val altText = "Jente som spiser melom. Grønn bakgrunn, rød melon. Fotografi."
  val contentString = s"[contentbrowser ==nid=$nodeId==imagecache=Fullbredde==width===alt=$altText==link===node_link=1==link_type=link_to_content==lightbox_size===remove_fields[76661]=1==remove_fields[76663]=1==remove_fields[76664]=1==remove_fields[76666]=1==insertion===link_title_text= ==link_text= ==text_align===css_class=contentbrowser contentbrowser]"
  val caption = "sample caption"
  val contentStringWithCaptions = s"[contentbrowser ==nid=$nodeId==imagecache=Fullbredde==width===alt=$altText==link===node_link=1==link_type=link_to_content==lightbox_size===remove_fields[76661]=1==remove_fields[76663]=1==remove_fields[76664]=1==remove_fields[76666]=1==insertion===link_title_text= ==link_text=$caption==text_align===css_class=contentbrowser contentbrowser]"
  val contentStringWithInsertionLink = s"[contentbrowser ==nid=$nodeId==imagecache=Fullbredde==width===alt=$altText==link===node_link=1==link_type=link_to_content==lightbox_size===remove_fields[76661]=1==remove_fields[76663]=1==remove_fields[76664]=1==remove_fields[76666]=1==insertion=link==link_title_text==link_text=$caption==text_align===css_class=contentbrowser contentbrowser]"

  test("That VideoConverter converts a ContentBrowser to html code") {
    val content = ContentBrowser(contentString, "nb")
    val expectedResult = s"""<$ResourceHtmlEmbedTag data-account="$NDLABrightcoveAccountId" data-caption="" data-player="$NDLABrightcovePlayerId" data-resource="brightcove" data-videoid="ref:${content.get("nid")}" />"""
    val Success((result, requiredLibraries, _)) = VideoConverter.convert(content, ImportStatus.empty)

    result should equal(expectedResult)
    requiredLibraries.length should equal(1)
  }

  test("Captions are added as video metadata") {
    val content = ContentBrowser(contentStringWithCaptions, "nb")
    val expectedResult = s"""<$ResourceHtmlEmbedTag data-account="$NDLABrightcoveAccountId" data-caption="$caption" data-player="$NDLABrightcovePlayerId" data-resource="brightcove" data-videoid="ref:${content.get("nid")}" />"""
    val Success((result, requiredLibraries, _)) = VideoConverter.convert(content, ImportStatus.empty)

    result should equal(expectedResult)
    requiredLibraries.length should equal(1)
  }

  test("the contentbrowser should be converted to a link if insertion method is link") {
    val content = ContentBrowser(contentStringWithInsertionLink, "nb")
    val expectedResult = s"""<$ResourceHtmlEmbedTag data-content-id="1" data-link-text="$caption" data-resource="${ResourceType.ContentLink}" />"""

    when(extractConvertStoreContent.processNode(nodeId, ImportStatus.empty)).thenReturn(Success(TestData.sampleApiArticle, ImportStatus.empty))

    val Success((result, requiredLibraries, _)) = VideoConverter.convert(content, ImportStatus.empty)
    result should equal(expectedResult)
    requiredLibraries.length should equal(0)
  }
}
