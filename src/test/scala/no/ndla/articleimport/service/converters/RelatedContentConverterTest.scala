/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters

import java.util.Date

import no.ndla.articleimport.caching.Memoize
import no.ndla.articleimport.integration.{MigrationEmbedMeta, MigrationRelatedContent}
import no.ndla.articleimport.model.domain.{ArticleTitle, ArticleType, ImportStatus, NodeToConvert}
import no.ndla.articleimport.{ArticleImportProperties, TestData, TestEnvironment, UnitSuite}
import no.ndla.validation.EmbedTagRules.ResourceHtmlEmbedTag
import no.ndla.validation.TagAttributes._
import no.ndla.validation.ResourceType._
import no.ndla.articleimport.model.api.{ImportException, ImportExceptions}
import org.mockito.Mockito._
import org.mockito.Matchers._

import scala.util.{Failure, Success}

class RelatedContentConverterTest extends UnitSuite with TestEnvironment {
  val relatedContent1 = MigrationRelatedContent("1234", "Tittel", "uri", 1)
  val relatedContent2 = MigrationRelatedContent("5678", "Palma", "uri", 1)
  val languageContent = TestData.sampleContent.copy(relatedContent = Seq(relatedContent1, relatedContent2))

  override def beforeEach() {
    when(extractService.getNodeType("1234")).thenReturn(Some("fagstoff"))
    when(extractService.getNodeType("5678")).thenReturn(Some("fagstoff"))
  }

  test("convert should insert a new section with an related-content embed tag") {
    val origContent = "<section><h1>hmm</h1></section>"

    when(
      extractConvertStoreContent
        .processNode(any[String], any[ImportStatus]))
      .thenReturn(Success((TestData.sampleApiArticle.copy(id = 1: Long), ImportStatus.empty)))
      .thenReturn(Success((TestData.sampleApiArticle.copy(id = 2), ImportStatus.empty)))

    val expectedContent = origContent + s"""<section><div data-type="$RelatedContent"><$ResourceHtmlEmbedTag $DataArticleId="1" $DataResource="$RelatedContent"><$ResourceHtmlEmbedTag $DataArticleId="2" $DataResource="$RelatedContent"></div></section>"""

    val Success((result, _)) =
      RelatedContentConverter.convert(languageContent.copy(content = origContent), ImportStatus.empty)
    result.content should equal(expectedContent)
  }

  test("convert should return a Failure if trying to link to a concept as related content") {
    when(extractConvertStoreContent.processNode(any[String], any[ImportStatus]))
      .thenReturn(Success((TestData.sampleApiConcept.copy(id = 1), ImportStatus.empty)))
      .thenReturn(Success((TestData.sampleApiArticle.copy(id = 2), ImportStatus.empty)))

    when(migrationApiClient.getAllTranslationNids(languageContent.nid))
      .thenReturn(Success(Set(languageContent.nid)))

    val Failure(result: ImportExceptions) =
      RelatedContentConverter.convert(languageContent, ImportStatus.empty)
    result.getMessage should equal(s"Failed to import node(s) with id(s) ${languageContent.nid}")
    result.errors should equal(
      List(
        ImportException(
          languageContent.nid,
          s"Related content with nid ${languageContent.nid} points to a concept. This should not be legal, no?")))
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
      .thenReturn(Success((TestData.sampleApiArticle.copy(id = 2), ImportStatus.empty)))

    val expectedContent = origContent + s"""<section><div data-type="$RelatedContent"><$ResourceHtmlEmbedTag data-resource="$RelatedContent" $DataTitle="$title" $DataUrl="$url"></div></section>"""

    val Success((result, _)) =
      RelatedContentConverter.convert(
        languageContent.copy(content = origContent, relatedContent = languageContent.relatedContent.slice(0, 1)),
        ImportStatus.empty)
    result.content should equal(expectedContent)
  }

}
