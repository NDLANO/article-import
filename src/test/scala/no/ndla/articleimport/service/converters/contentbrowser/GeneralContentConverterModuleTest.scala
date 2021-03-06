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
import org.mockito.ArgumentMatchers._

import scala.util.{Failure, Success, Try}

class GeneralContentConverterModuleTest extends UnitSuite with TestEnvironment {
  val (nodeId, nodeId2) = ("1234", "4321")
  val insertion = "inline"
  val altText = "Jente som spiser melom. Grønn bakgrunn, rød melon. Fotografi."

  val content = TestData.contentBrowserWithFields(List.empty,
                                                  "nid" -> nodeId,
                                                  "alt" -> altText,
                                                  "insertion" -> insertion,
                                                  "link_text" -> "Tittel")

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

  val generalContentConverter = new GeneralContentConverterModule {
    override val typeName: String = "test"
  }

  override def beforeEach = {
    when(extractConvertStoreContent.getMainNodeId(any[String]))
      .thenAnswer((invocation: InvocationOnMock) => Some(invocation.getArgument[String](0)))
    when(extractService.getNodeData(any[String])).thenReturn(Success(sampleNodeToConvert))
  }

  test("That GeneralContentConverter returns the contents according to language") {
    when(extractService.getNodeGeneralContent(nodeId))
      .thenReturn(Seq(sampleFagstoff1, sampleFagstoff2))
    val Success((result, requiredLibraries, status)) =
      generalContentConverter.convert(content, ImportStatus.empty)

    result should equal(sampleFagstoff1.content)
    status.messages.isEmpty should equal(true)
    requiredLibraries.isEmpty should equal(true)
  }

  test("That GeneralContentConverter returns a Failure when the node is not found") {
    when(extractService.getNodeGeneralContent(nodeId)).thenReturn(Seq())
    generalContentConverter
      .convert(content, ImportStatus.empty)
      .isFailure should be(true)
  }

  test("That GeneralContentConverter inserts the content if insertion mode is 'collapsed_body'") {
    val content = TestData.contentBrowserWithFields(List.empty,
                                                    "nid" -> nodeId,
                                                    "alt" -> altText,
                                                    "insertion" -> "collapsed_body",
                                                    "link_text" -> "Tittel")
    val expectedResult = s"<details><summary>Tittel</summary>${sampleFagstoff1.content}</details>"

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
    val content = TestData.contentBrowserWithFields(List.empty,
                                                    "nid" -> nodeId,
                                                    "alt" -> altText,
                                                    "insertion" -> "link",
                                                    "link_text" -> "Tittel")
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
    val content = TestData.contentBrowserWithFields(List.empty,
                                                    "nid" -> nodeId,
                                                    "alt" -> altText,
                                                    "insertion" -> "lightbox_large",
                                                    "link_text" -> "Tittel")
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
    val content = TestData.contentBrowserWithFields(List.empty,
                                                    "nid" -> nodeId,
                                                    "alt" -> altText,
                                                    "insertion" -> "unknown",
                                                    "link_text" -> "Tittel")
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
    val content = TestData.contentBrowserWithFields(List.empty,
                                                    "nid" -> nodeId,
                                                    "alt" -> altText,
                                                    "insertion" -> "link",
                                                    "link_text" -> "Tittel")
    val expectedResult =
      s""" <$ResourceHtmlEmbedTag data-content-id="1111" data-link-text="Tittel" data-resource="content-link" />"""

    when(extractService.getNodeGeneralContent(nodeId))
      .thenReturn(Seq(sampleFagstoff1, sampleFagstoff2))
    when(draftApiClient.getArticleIdFromExternalId(nodeId)).thenReturn(None)
    when(draftApiClient.getConceptIdFromExternalId(nodeId)).thenReturn(None)
    when(extractConvertStoreContent.processNode(nodeId, ImportStatus(Seq(), Seq(), Set(nodeId2))))
      .thenReturn(
        Try((TestData.sampleApiArticle.copy(id = newNodeid), ImportStatus(Seq(), Seq(), Set(nodeId2, nodeId)))))

    val Success((result, _, status)) =
      generalContentConverter.convert(content, ImportStatus(Seq.empty, Seq.empty, Set(nodeId2)))

    result should equal(expectedResult)
    status.messages should be(List.empty)
    status.visitedNodes should be(Set(nodeId2, nodeId))
  }

  test("That GeneralContentConverter returns a Failure if node could not be imported") {
    val content = TestData.contentBrowserWithFields(List.empty,
                                                    "nid" -> nodeId,
                                                    "alt" -> altText,
                                                    "insertion" -> "link",
                                                    "link_title" -> "Tittel")
    val expectedResult =
      s""" <a href="http://ndla.no/node/$nodeId" title="">Tittel</a>"""

    when(extractService.getNodeGeneralContent(nodeId))
      .thenReturn(Seq(sampleFagstoff1, sampleFagstoff2))
    when(draftApiClient.getArticleIdFromExternalId(nodeId)).thenReturn(None)
    when(draftApiClient.getConceptIdFromExternalId(nodeId)).thenReturn(None)
    when(extractConvertStoreContent.processNode(nodeId, ImportStatus(Seq(), Seq(), Set(nodeId2))))
      .thenReturn(Failure(NotFoundException("Node was not found")))

    generalContentConverter
      .convert(content, ImportStatus(Seq.empty, Seq.empty, Set(nodeId2)))
      .isFailure should be(true)
  }

  test("That GeneralContentConverter inserts the content in language before languageless") {
    val content = TestData.contentBrowserWithFields(List.empty,
                                                    "nid" -> nodeId,
                                                    "alt" -> altText,
                                                    "insertion" -> "collapsed_body",
                                                    "link_text" -> "Tittel")
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
    val content = TestData.contentBrowserWithFields(List.empty,
                                                    "nid" -> nodeId,
                                                    "alt" -> altText,
                                                    "insertion" -> "collapsed_body",
                                                    "link_text" -> "Tittel")
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
      TestData.contentBrowserWithFields(List.empty,
                                        "nid" -> nodeId,
                                        "insertion" -> "lightbox_custom",
                                        "link_text" -> "link")
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

  test("That licenses and authors should not be inserted for links") {
    val content =
      TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "insertion" -> "link", "link_text" -> "link")
    when(extractService.getNodeGeneralContent(nodeId)).thenReturn(Seq(sampleFagstoff1, sampleFagstoff2))
    when(draftApiClient.getArticleIdFromExternalId(nodeId)).thenReturn(Some(sampleArticle.id))
    val Success((_, _, status)) = generalContentConverter.convert(content, ImportStatus.empty)

    status.nodeLocalContext.insertedAuthors should be(List.empty)
    status.nodeLocalContext.insertedLicenses should be(List.empty)
  }

  test("That licenses and authors should be inserted for inline content") {
    val content1 =
      TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "insertion" -> "inline", "link_text" -> "jadda")
    val content2 =
      TestData.contentBrowserWithFields(List.empty,
                                        "nid" -> nodeId,
                                        "insertion" -> "collapsed_body",
                                        "link_text" -> "jadda")
    when(extractService.getNodeGeneralContent(nodeId)).thenReturn(Seq(sampleFagstoff1, sampleFagstoff2))
    when(draftApiClient.getArticleIdFromExternalId(nodeId)).thenReturn(Some(sampleArticle.id))
    when(extractService.getNodeData(any[String])).thenReturn(Success(
      sampleNodeToConvert.copy(license = Some("by-sa-nc"), authors = Seq(Author("opphavsmann", "Johnfinn Arvidsen")))))

    val Success((_, _, status1)) = generalContentConverter.convert(content1, ImportStatus.empty)
    val Success((_, _, status2)) = generalContentConverter.convert(content2, ImportStatus.empty)

    status1.nodeLocalContext.insertedAuthors should be(List(Author("opphavsmann", "Johnfinn Arvidsen")))
    status1.nodeLocalContext.insertedLicenses should be(List("by-sa-nc"))
    status2.nodeLocalContext.insertedAuthors should be(List(Author("opphavsmann", "Johnfinn Arvidsen")))
    status2.nodeLocalContext.insertedLicenses should be(List("by-sa-nc"))
  }
}
