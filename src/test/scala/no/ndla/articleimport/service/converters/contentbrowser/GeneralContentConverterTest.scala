/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters.contentbrowser

import java.util.Date

import no.ndla.articleimport.{TestData, TestEnvironment, UnitSuite}
import no.ndla.validation.EmbedTagRules.ResourceHtmlEmbedTag
import no.ndla.articleimport.model.api.NotFoundException
import no.ndla.articleimport.model.domain._
import org.mockito.Mockito._
import org.mockito.invocation.InvocationOnMock
import org.mockito.Matchers._

import scala.util.{Failure, Success, Try}

class GeneralContentConverterTest extends UnitSuite with TestEnvironment {
  val (nodeId, nodeId2) = ("1234", "4321")
  val insertion = "inline"
  val altText = "Jente som spiser melom. Grønn bakgrunn, rød melon. Fotografi."

  val contentString =
    s"[contentbrowser ==nid=$nodeId==imagecache=Fullbredde==width===alt=$altText==link===node_link=1==link_type=link_to_content==lightbox_size===remove_fields[76661]=1==remove_fields[76663]=1==remove_fields[76664]=1==remove_fields[76666]=1==insertion=$insertion==link_title_text= ==link_text= ==text_align===css_class=contentbrowser contentbrowser]"

  val sampleFagstoff1 =
    NodeGeneralContent(nodeId, nodeId, "Tittel", "Innhold", "nb")

  val sampleFagstoff2 =
    NodeGeneralContent(nodeId, nodeId2, "Tittel", "Innhald", "nn")

  val sampleNodeToConvert = NodeToConvert(Seq(ArticleTitle("title", "en")),
                                          Seq(),
                                          Some("publicdomain"),
                                          Seq(),
                                          Seq(),
                                          "fagstoff",
                                          "fagstoff",
                                          new Date(0),
                                          new Date(1),
                                          ArticleType.Standard,
                                          Seq.empty)

  val sampleContent =
    TestData.sampleContent.copy(content = "<div>sample content</div>")
  val sampleArticle = TestData.sampleApiArticle

  val generalContentConverter = new GeneralContentConverter {
    override val typeName: String = "test"
  }

  override def beforeEach = {
    when(extractConvertStoreContent.getMainNodeId(any[String]))
      .thenAnswer((invocation: InvocationOnMock) => Some(invocation.getArgumentAt(0, classOf[String])))
  }

  test("That GeneralContentConverter returns the contents according to language") {
    val content = ContentBrowserString(contentString, "nb")

    when(extractService.getNodeGeneralContent(nodeId))
      .thenReturn(Seq(sampleFagstoff1, sampleFagstoff2))
    val Success((result, requiredLibraries, status)) =
      generalContentConverter.convert(content, ImportStatus.empty)

    result should equal(sampleFagstoff1.content)
    status.messages.isEmpty should equal(true)
    requiredLibraries.isEmpty should equal(true)
  }

  test("That GeneralContentConverter returns a Failure when the node is not found") {
    val content = ContentBrowserString(contentString, "nb")

    when(extractService.getNodeGeneralContent(nodeId)).thenReturn(Seq())
    generalContentConverter
      .convert(content, ImportStatus.empty)
      .isFailure should be(true)
  }

  test("That GeneralContentConverter inserts the content if insertion mode is 'collapsed_body'") {
    val contentString =
      s"[contentbrowser ==nid=$nodeId==imagecache=Fullbredde==width===alt=$altText==link===node_link=1==link_type=link_to_content==lightbox_size===remove_fields[76661]=1==remove_fields[76663]=1==remove_fields[76664]=1==remove_fields[76666]=1==insertion=collapsed_body==link_title_text===link_text=Tittel==text_align===css_class=contentbrowser contentbrowser]"
    val content = ContentBrowserString(contentString, "nb")
    val expectedResult =
      s"<details><summary>Tittel</summary>${sampleFagstoff1.content}</details>"

    when(extractService.getNodeGeneralContent(nodeId))
      .thenReturn(Seq(sampleFagstoff1, sampleFagstoff2))
    when(draftApiClient.getArticleIdFromExternalId(nodeId))
      .thenReturn(Some(sampleArticle.id))
    when(draftApiClient.getConceptIdFromExternalId(nodeId)).thenReturn(None)
    val Success((result, requiredLibraries, status)) =
      generalContentConverter.convert(content, ImportStatus.empty)

    result should equal(expectedResult)
    status.messages.isEmpty should equal(true)
    requiredLibraries.isEmpty should equal(true)
  }

  test("That GeneralContentConverter inserts the content if insertion mode is 'link'") {
    val contentString =
      s"[contentbrowser ==nid=$nodeId==imagecache=Fullbredde==width===alt=$altText==link===node_link=1==link_type=link_to_content==lightbox_size===remove_fields[76661]=1==remove_fields[76663]=1==remove_fields[76664]=1==remove_fields[76666]=1==insertion=link==link_title_text===link_text=Tittel==text_align===css_class=contentbrowser contentbrowser]"
    val content = ContentBrowserString(contentString, "nb")
    val expectedResult =
      s""" <$ResourceHtmlEmbedTag data-content-id="1" data-link-text="Tittel" data-resource="content-link" />"""

    when(extractService.getNodeGeneralContent(nodeId))
      .thenReturn(Seq(sampleFagstoff1, sampleFagstoff2))
    when(draftApiClient.getArticleIdFromExternalId(nodeId))
      .thenReturn(Some(sampleArticle.id))
    when(draftApiClient.getConceptIdFromExternalId(nodeId)).thenReturn(None)
    val Success((result, requiredLibraries, status)) =
      generalContentConverter.convert(content, ImportStatus.empty)

    result should equal(expectedResult)
    status.messages.isEmpty should equal(true)
    requiredLibraries.isEmpty should equal(true)
  }

  test("That GeneralContentConverter inserts the content if insertion mode is 'lightbox_large'") {
    val contentString =
      s"[contentbrowser ==nid=$nodeId==imagecache=Fullbredde==width===alt=$altText==link===node_link=1==link_type=link_to_content==lightbox_size===remove_fields[76661]=1==remove_fields[76663]=1==remove_fields[76664]=1==remove_fields[76666]=1==insertion=lightbox_large==link_title_text===link_text=Tittel==text_align===css_class=contentbrowser contentbrowser]"
    val content = ContentBrowserString(contentString, "nb")
    val expectedResult =
      s""" <$ResourceHtmlEmbedTag data-content-id="1" data-link-text="Tittel" data-open-in="new-context" data-resource="content-link" />"""

    when(extractService.getNodeGeneralContent(nodeId))
      .thenReturn(Seq(sampleFagstoff1, sampleFagstoff2))
    when(draftApiClient.getArticleIdFromExternalId(nodeId))
      .thenReturn(Some(sampleArticle.id))
    when(draftApiClient.getConceptIdFromExternalId(nodeId)).thenReturn(None)
    val Success((result, requiredLibraries, status)) =
      generalContentConverter.convert(content, ImportStatus.empty)

    result should equal(expectedResult)
    status.messages.isEmpty should equal(true)
    requiredLibraries.isEmpty should equal(true)
  }

  test("That GeneralContentConverter defaults to 'link' if the insertion method is unknown") {
    val contentString =
      s"[contentbrowser ==nid=$nodeId==imagecache=Fullbredde==width===alt=$altText==link===node_link=1==link_type=link_to_content==lightbox_size===remove_fields[76661]=1==remove_fields[76663]=1==remove_fields[76664]=1==remove_fields[76666]=1==insertion=unknown==link_title_text===link_text=Tittel==text_align===css_class=contentbrowser contentbrowser]"
    val content = ContentBrowserString(contentString, "nb")
    val expectedResult =
      s""" <$ResourceHtmlEmbedTag data-content-id="1" data-link-text="Tittel" data-resource="content-link" />"""

    when(extractService.getNodeGeneralContent(nodeId))
      .thenReturn(Seq(sampleFagstoff1, sampleFagstoff2))
    when(draftApiClient.getArticleIdFromExternalId(nodeId))
      .thenReturn(Some(sampleArticle.id))
    when(draftApiClient.getConceptIdFromExternalId(nodeId)).thenReturn(None)

    val Success((result, requiredLibraries, status)) =
      generalContentConverter.convert(content, ImportStatus.empty)

    result should equal(expectedResult)
    status.messages.nonEmpty should equal(true)
    requiredLibraries.isEmpty should equal(true)
  }

  test("That GeneralContentConverter imports nodes from old NDLA which is referenced in a content") {
    val newNodeid: Long = 1111
    val contentString =
      s"[contentbrowser ==nid=$nodeId==imagecache=Fullbredde==width===alt=$altText==link===node_link=1==link_type=link_to_content==lightbox_size===remove_fields[76661]=1==remove_fields[76663]=1==remove_fields[76664]=1==remove_fields[76666]=1==insertion=link==link_title_text===link_text=Tittel==text_align===css_class=contentbrowser contentbrowser]"
    val content = ContentBrowserString(contentString, "nb")
    val expectedResult =
      s""" <$ResourceHtmlEmbedTag data-content-id="1111" data-link-text="Tittel" data-resource="content-link" />"""

    when(extractService.getNodeGeneralContent(nodeId))
      .thenReturn(Seq(sampleFagstoff1, sampleFagstoff2))
    when(draftApiClient.getArticleIdFromExternalId(nodeId)).thenReturn(None)
    when(draftApiClient.getConceptIdFromExternalId(nodeId)).thenReturn(None)
    when(extractConvertStoreContent.processNode(nodeId, ImportStatus(Seq(), Set(nodeId2))))
      .thenReturn(Try((TestData.sampleApiArticle.copy(id = newNodeid), ImportStatus(Seq(), Set(nodeId2, nodeId)))))

    val Success((result, _, status)) =
      generalContentConverter.convert(content, ImportStatus(Seq.empty, Set(nodeId2)))

    result should equal(expectedResult)
    status should equal(ImportStatus(List(), Set(nodeId2, nodeId)))
  }

  test("That GeneralContentConverter returns a Failure if node could not be imported") {
    val contentString =
      s"[contentbrowser ==nid=$nodeId==imagecache=Fullbredde==width===alt=$altText==link===node_link=1==link_type=link_to_content==lightbox_size===remove_fields[76661]=1==remove_fields[76663]=1==remove_fields[76664]=1==remove_fields[76666]=1==insertion=link==link_title_text===link_text=Tittel==text_align===css_class=contentbrowser contentbrowser]"
    val content = ContentBrowserString(contentString, "nb")
    val expectedResult =
      s""" <a href="http://ndla.no/node/$nodeId" title="">Tittel</a>"""

    when(extractService.getNodeGeneralContent(nodeId))
      .thenReturn(Seq(sampleFagstoff1, sampleFagstoff2))
    when(draftApiClient.getArticleIdFromExternalId(nodeId)).thenReturn(None)
    when(draftApiClient.getConceptIdFromExternalId(nodeId)).thenReturn(None)
    when(extractConvertStoreContent.processNode(nodeId, ImportStatus(Seq(), Set(nodeId2))))
      .thenReturn(Failure(NotFoundException("Node was not found")))

    generalContentConverter
      .convert(content, ImportStatus(Seq.empty, Set(nodeId2)))
      .isFailure should be(true)
  }

  test("That GeneralContentConverter inserts the content in language before languageless") {
    val contentString =
      s"[contentbrowser ==nid=$nodeId==imagecache=Fullbredde==width===alt=$altText==link===node_link=1==link_type=link_to_content==lightbox_size===remove_fields[76661]=1==remove_fields[76663]=1==remove_fields[76664]=1==remove_fields[76666]=1==insertion=collapsed_body==link_title_text===link_text=Tittel==text_align===css_class=contentbrowser contentbrowser]"
    val content = ContentBrowserString(contentString, "nb")
    val fagStoffWithNoLang =
      sampleFagstoff2.copy(language = Language.NoLanguage)
    val expectedResult =
      s"<details><summary>Tittel</summary>${sampleFagstoff1.content}</details>"

    when(extractService.getNodeGeneralContent(nodeId))
      .thenReturn(Seq(sampleFagstoff1, fagStoffWithNoLang))
    when(draftApiClient.getContentByExternalId(nodeId))
      .thenReturn(Some(sampleArticle))
    val Success((result, requiredLibraries, status)) =
      generalContentConverter.convert(content, ImportStatus.empty)

    result should equal(expectedResult)
    status.messages.isEmpty should equal(true)
    requiredLibraries.isEmpty should equal(true)
  }

  test("That GeneralContentConverter inserts the content languageless if language not found") {
    val contentString =
      s"[contentbrowser ==nid=$nodeId==imagecache=Fullbredde==width===alt=$altText==link===node_link=1==link_type=link_to_content==lightbox_size===remove_fields[76661]=1==remove_fields[76663]=1==remove_fields[76664]=1==remove_fields[76666]=1==insertion=collapsed_body==link_title_text===link_text=Tittel==text_align===css_class=contentbrowser contentbrowser]"
    val content = ContentBrowserString(contentString, "nb")
    val fagStoffWithWrongLang1 = sampleFagstoff2.copy(language = "en")
    val fagStoffWithWrongLang2 = sampleFagstoff2.copy(language = "nn")
    val fagStoffWithWrongLang3 = sampleFagstoff2.copy(language = "fr")
    val fagStoffWithWrongLang4 = sampleFagstoff2.copy(language = "de")
    val fagStoffWithNoLang =
      sampleFagstoff1.copy(language = Language.NoLanguage, content = "languageNeutral")
    val expectedResult =
      s"<details><summary>Tittel</summary>${fagStoffWithNoLang.content}</details>"

    when(extractService.getNodeGeneralContent(nodeId)).thenReturn(
      Seq(fagStoffWithWrongLang1,
          fagStoffWithNoLang,
          fagStoffWithWrongLang2,
          fagStoffWithWrongLang3,
          fagStoffWithWrongLang4))
    when(draftApiClient.getContentByExternalId(nodeId))
      .thenReturn(Some(sampleArticle))
    val Success((result, requiredLibraries, status)) =
      generalContentConverter.convert(content, ImportStatus.empty)

    result should equal(expectedResult)
    status.messages.isEmpty should equal(true)
    requiredLibraries.isEmpty should equal(true)
  }

  test("GeneralContentConverter should insert an open-in-new-window cnotent-link embed on all lightbox links") {
    val content =
      TestData.contentBrowserWithFields("nid" -> nodeId, "insertion" -> "lightbox_custom", "link_text" -> "link")
    when(extractService.getNodeGeneralContent(nodeId))
      .thenReturn(Seq(sampleFagstoff1, sampleFagstoff2))
    when(draftApiClient.getArticleIdFromExternalId(nodeId))
      .thenReturn(Some(sampleArticle.id))
    val Success((res, requiredLibs, status)) =
      generalContentConverter.convert(content, ImportStatus.empty)

    val expectedResult =
      """ <embed data-content-id="1" data-link-text="link" data-open-in="new-context" data-resource="content-link" />"""
    res should equal(expectedResult)
    requiredLibs.isEmpty should be(true)
    status.messages.isEmpty should be(true)
  }

}
