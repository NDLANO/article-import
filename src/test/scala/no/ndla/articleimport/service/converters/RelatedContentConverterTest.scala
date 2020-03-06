/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters

import java.util.Date

import no.ndla.articleimport.integration.{MigrationEmbedMeta, MigrationRelatedContent}
import no.ndla.articleimport.model.api.ImportException
import no.ndla.articleimport.model.domain.{ArticleTitle, ArticleType, ImportStatus, NodeToConvert}
import no.ndla.articleimport.{ArticleImportProperties, TestData, TestEnvironment, UnitSuite}
import no.ndla.validation.EmbedTagRules.ResourceHtmlEmbedTag
import no.ndla.validation.ResourceType._
import no.ndla.validation.TagAttributes._
import org.mockito.ArgumentMatchers.{eq => eqTo, _}
import org.mockito.Mockito._
import org.mockito.invocation.InvocationOnMock

import scala.util.{Failure, Success}

class RelatedContentConverterTest extends UnitSuite with TestEnvironment {
  val relatedContent1 = MigrationRelatedContent("1234", "Tittel", "uri", 1)
  val relatedContent2 = MigrationRelatedContent("5678", "Palma", "uri", 1)
  val languageContent = TestData.sampleContent.copy(relatedContent = Seq(relatedContent1, relatedContent2))

  override def beforeEach(): Unit = {
    when(extractService.getNodeType("1234")).thenReturn(Some("fagstoff"))
    when(extractService.getNodeType("5678")).thenReturn(Some("fagstoff"))
    when(taxonomyApiClient.getResource(any[String])).thenReturn(Success(Some(TestData.sampleTaxonomyResource)))
    when(taxonomyApiClient.getTopic(any[String])).thenReturn(Success(Some(TestData.sampleTaxonomyTopic)))
    when(extractService.getNodeData(any[String])).thenAnswer((i: InvocationOnMock) => {
      val nid = i.getArgument[String](0)
      Success(
        TestData.sampleNodeToConvert.copy(
          contents = Seq(
            TestData.sampleContent.copy(nid = nid, tnid = nid)
          )))
    })
  }

  test("convert should insert a new section with an related-content embed tag") {
    val origContent = "<section><h1>hmm</h1></section>"
    when(extractConvertStoreContent.processNode(any[String], any[ImportStatus]))
      .thenReturn(Success((TestData.sampleApiArticle.copy(id = 1: Long), ImportStatus.empty)))
      .andThen(Success((TestData.sampleApiArticle.copy(id = 2), ImportStatus.empty)))

    val expectedContent = origContent + s"""<section><div data-type="$RelatedContent"><$ResourceHtmlEmbedTag $DataArticleId="1" $DataResource="$RelatedContent"><$ResourceHtmlEmbedTag $DataArticleId="2" $DataResource="$RelatedContent"></div></section>"""

    val Success((result, _)) =
      RelatedContentConverter.convert(languageContent.copy(content = origContent), ImportStatus.empty)
    result.content should equal(expectedContent)
  }

  test("convert should return a Success with error in importStatus if trying to link to a concept as related content") {
    when(extractConvertStoreContent.processNode(any[String], any[ImportStatus]))
      .thenAnswer((i: InvocationOnMock) => {
        Success((TestData.sampleApiConcept.copy(id = 1), i.getArgument[ImportStatus](1)))
      })
      .andThenAnswer((i: InvocationOnMock) => {
        Success((TestData.sampleApiArticle.copy(id = 2), i.getArgument[ImportStatus](1)))
      })

    when(migrationApiClient.getAllTranslationNids(languageContent.nid))
      .thenReturn(Success(Set(languageContent.nid)))

    val Success((result, status)) =
      RelatedContentConverter.convert(languageContent, ImportStatus.empty)

    val expectedContent =
      s"""${languageContent.content}<section><div data-type="related-content"><embed data-article-id="2" data-resource="related-content"></div></section>"""

    result.content should be(expectedContent)
    status.errors should be(
      List(
        ImportException(
          languageContent.nid,
          s"Related content with nid ${languageContent.nid} points to a concept. This should not be legal, no?")))
  }

  test("convert should return a success with error in importStatus if unsupported link as related") {

    when(extractService.getNodeType(any[String])).thenReturn(Some("unsupported"))

    when(extractConvertStoreContent.processNode(any[String], any[ImportStatus]))
      .thenAnswer((i: InvocationOnMock) => {
        Success((TestData.sampleApiArticle.copy(id = 1), i.getArgument[ImportStatus](1)))
      })
      .andThenAnswer((i: InvocationOnMock) => {
        Success((TestData.sampleApiArticle.copy(id = 2), i.getArgument[ImportStatus](1)))
      })

    when(migrationApiClient.getAllTranslationNids(languageContent.nid))
      .thenReturn(Success(Set(languageContent.nid)))

    val Success((result, status)) =
      RelatedContentConverter.convert(languageContent, ImportStatus.empty)

    result.content should be(languageContent.content)
    status.errors should be(
      List(
        ImportException("1234",
                        s"Related content with node id 1234 (unsupported) is unsupported and will not be imported."),
        ImportException("5678",
                        s"Related content with node id 5678 (unsupported) is unsupported and will not be imported.")
      ))
  }

  test("convert should still import one if one out of two related fails") {
    when(extractService.getNodeType("1234")).thenReturn(Some("unsupported"))
    when(extractService.getNodeType("5678")).thenReturn(Some("fagstoff"))

    when(extractConvertStoreContent.processNode(eqTo("1234"), any[ImportStatus]))
      .thenAnswer((i: InvocationOnMock) => {
        Success((TestData.sampleApiArticle.copy(id = 1), i.getArgument[ImportStatus](1)))
      })
    when(extractConvertStoreContent.processNode(eqTo("5678"), any[ImportStatus]))
      .thenAnswer((i: InvocationOnMock) => {
        Success((TestData.sampleApiArticle.copy(id = 2), i.getArgument[ImportStatus](1)))
      })

    when(migrationApiClient.getAllTranslationNids(languageContent.nid))
      .thenReturn(Success(Set(languageContent.nid)))

    val Success((result, status)) =
      RelatedContentConverter.convert(languageContent, ImportStatus.empty)

    val expectedContent =
      s"""${languageContent.content}<section><div data-type="related-content"><embed data-article-id="2" data-resource="related-content"></div></section>"""

    result.content should be(expectedContent)
    status.errors should be(
      List(
        ImportException("1234",
                        s"Related content with node id 1234 (unsupported) is unsupported and will not be imported.")
      ))

  }

  test("convert should not add a new section if there are no related contents") {
    val origContent = "<section><h1>hmm</h1></section>"

    val Success((result, _)) =
      RelatedContentConverter.convert(languageContent.copy(content = origContent, relatedContent = Seq.empty),
                                      ImportStatus.empty)
    result.content should equal(origContent)
  }

  test("convert should not add related nodes of unsupported types") {
    val origContent = "<section><h1>hmm</h1></section>"

    when(extractService.getNodeType("5678")).thenReturn(Some("link"))
    when(extractConvertStoreContent.processNode(any[String], any[ImportStatus]))
      .thenReturn(Success((TestData.sampleApiArticle.copy(id = 1), ImportStatus.empty)))

    val expectedContent = origContent + s"""<section><div data-type="$RelatedContent"><$ResourceHtmlEmbedTag $DataArticleId="1" $DataResource="$RelatedContent"></div></section>"""

    val Success((result, _)) =
      RelatedContentConverter.convert(languageContent.copy(content = origContent), ImportStatus.empty)
    result.content should equal(expectedContent)
  }

  test("That convert should only import related content that is in taxonomy") {

    when(taxonomyApiClient.getResource("1234")).thenReturn(Success(None))
    when(taxonomyApiClient.getTopic("1234")).thenReturn(Success(None))
    when(taxonomyApiClient.getResource("5678")).thenReturn(Success(Some(TestData.sampleTaxonomyResource)))
    val origContent = "<section><h1>hmm</h1></section>"

    when(extractConvertStoreContent.processNode(any[String], any[ImportStatus]))
      .thenReturn(Success((TestData.sampleApiArticle.copy(id = 1), ImportStatus.empty)))
      .andThen(Success((TestData.sampleApiArticle.copy(id = 2), ImportStatus.empty)))

    val expectedContent = origContent + s"""<section><div data-type="$RelatedContent"><$ResourceHtmlEmbedTag $DataArticleId="1" $DataResource="$RelatedContent"></div></section>"""

    val Success((result, _)) =
      RelatedContentConverter.convert(languageContent.copy(content = origContent), ImportStatus.empty)
    result.content should equal(expectedContent)
  }

  test("That convert succeeds with a different error if taxonomy fails") {
    val taxonomyException = new RuntimeException("Taxonomy failed blabla")
    when(taxonomyApiClient.getResource("1234")).thenReturn(Failure(taxonomyException))
    when(taxonomyApiClient.getTopic("1234")).thenReturn(Failure(taxonomyException))
    when(taxonomyApiClient.getResource("5678")).thenReturn(Success(None))
    when(taxonomyApiClient.getTopic("5678")).thenReturn(Success(Some(TestData.sampleTaxonomyTopic)))
    val origContent = "<section><h1>hmm</h1></section>"

    val expectedContent = origContent + s"""<section><div data-type="$RelatedContent"><$ResourceHtmlEmbedTag $DataArticleId="2" $DataResource="$RelatedContent"></div></section>"""

    when(extractConvertStoreContent.processNode(eqTo("1234"), any[ImportStatus]))
      .thenReturn(Success((TestData.sampleApiArticle.copy(id = 1), ImportStatus.empty)))
    when(extractConvertStoreContent.processNode(eqTo("5678"), any[ImportStatus]))
      .thenReturn(Success((TestData.sampleApiArticle.copy(id = 2), ImportStatus.empty)))

    val Success((result, _)) =
      RelatedContentConverter.convert(languageContent.copy(content = origContent), ImportStatus.empty)
    result.content should equal(expectedContent)
  }

  test("Convert should add related link node without embedcode as a direct link in the embed") {
    val origContent = "<section><h1>hmm</h1></section>"

    val url = "https://example.com"
    val title = "Title is here"

    when(extractService.getNodeType(any[String])).thenReturn(Some(ArticleImportProperties.nodeTypeLink))
    when(extractService.getLinkEmbedMeta(any[String])).thenReturn(Success(MigrationEmbedMeta(Some(url), None)))
    val relatedUrlNode = NodeToConvert(
      titles = Seq(ArticleTitle(title, "nb")),
      contents = Seq.empty,
      license = None,
      authors = Seq.empty,
      tags = Seq.empty,
      nodeType = ArticleImportProperties.nodeTypeLink,
      contentType = "123",
      created = new Date(),
      updated = new Date(),
      articleType = ArticleType.Standard,
      editorialKeywords = Seq.empty
    )
    when(extractService.getNodeData(any[String])).thenReturn(Success(relatedUrlNode))

    when(
      extractConvertStoreContent
        .processNode(any[String], any[ImportStatus]))
      .thenReturn(Success((TestData.sampleApiArticle.copy(id = 1: Long), ImportStatus.empty)))
      .andThen(Success((TestData.sampleApiArticle.copy(id = 2), ImportStatus.empty)))

    val expectedContent = origContent + s"""<section><div data-type="$RelatedContent"><$ResourceHtmlEmbedTag data-resource="$RelatedContent" $DataTitle="$title" $DataUrl="$url"></div></section>"""

    val Success((result, status)) =
      RelatedContentConverter.convert(
        languageContent.copy(content = origContent, relatedContent = languageContent.relatedContent.slice(0, 1)),
        ImportStatus.empty)
    result.content should equal(expectedContent)
    status.errors should be(Seq.empty)
  }

  test("That mainNodeId is used to look up taxonomy for related content") {
    val origContent = "<section><h1>hmm</h1></section>"
    val relatedMainNid = "9990"
    val relatedNotMainNid = "9995"

    when(
      extractConvertStoreContent
        .processNode(any[String], any[ImportStatus]))
      .thenReturn(Success((TestData.sampleApiArticle.copy(id = 1: Long), ImportStatus.empty)))
      .andThen(Success((TestData.sampleApiArticle.copy(id = 2), ImportStatus.empty)))

    when(extractService.getNodeType(relatedNotMainNid)).thenReturn(Some("fagstoff"))

    when(extractService.getNodeData(relatedNotMainNid)).thenReturn(
      Success(
        TestData.sampleNodeToConvert.copy(
          contents = List(
            TestData.sampleContent.copy(nid = relatedMainNid, tnid = "0"),
            TestData.sampleContent.copy(nid = relatedNotMainNid, tnid = relatedMainNid)
          ))
      )
    )

    RelatedContentConverter.convert(
      languageContent.copy(content = origContent, relatedContent = Seq(relatedContent1.copy(nid = relatedNotMainNid))),
      ImportStatus.empty)

    verify(taxonomyApiClient, times(1)).getResource(relatedMainNid)
  }

}
