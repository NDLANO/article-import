/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters

import no.ndla.articleimport.caching.Memoize
import no.ndla.articleimport.integration.MigrationRelatedContent
import no.ndla.articleimport.model.domain.ImportStatus
import no.ndla.articleimport.{TestData, TestEnvironment, UnitSuite}
import no.ndla.validation.EmbedTagRules.ResourceHtmlEmbedTag
import no.ndla.validation.TagAttributes._
import no.ndla.validation.ResourceType._
import no.ndla.articleimport.model.api.{ImportException, ImportExceptions, TaxonomyException}
import org.mockito.Mockito._
import org.mockito.Matchers._
import org.mockito.Matchers
import org.mockito.invocation.InvocationOnMock

import scala.util.{Failure, Success}

class RelatedContentConverterTest extends UnitSuite with TestEnvironment {
  val relatedContent1 = MigrationRelatedContent("1234", "Tittel", "uri", 1)
  val relatedContent2 = MigrationRelatedContent("5678", "Palma", "uri", 1)
  val languageContent = TestData.sampleContent.copy(relatedContent = Seq(relatedContent1, relatedContent2))

  override def beforeEach() {
    when(extractService.getNodeType("1234")).thenReturn(Some("fagstoff"))
    when(extractService.getNodeType("5678")).thenReturn(Some("fagstoff"))
    when(taxonomyApiClient.existsInTaxonomy(any[String])).thenReturn(Success(true))
    when(extractService.getNodeData(any[String])).thenAnswer((i: InvocationOnMock) => {
      val nid = i.getArgumentAt(0, "".getClass)
      Success(
        TestData.sampleNodeToConvert.copy(
          contents = Seq(
            TestData.sampleContent.copy(nid = nid, tnid = nid)
          )))
    })
  }

  test("convert should insert a new section with an related-content embed tag") {
    val origContent = "<section><h1>hmm</h1></section>"
    when(
      extractConvertStoreContent
        .processNode(any[String], any[ImportStatus]))
      .thenReturn(Success((TestData.sampleApiArticle.copy(id = 1: Long), ImportStatus.empty)))
      .thenReturn(Success((TestData.sampleApiArticle.copy(id = 2), ImportStatus.empty)))

    val expectedContent = origContent + s"""<section><$ResourceHtmlEmbedTag $DataArticleIds="1,2" $DataResource="$RelatedContent"></section>"""

    val Success((result, _)) =
      RelatedContentConverter.convert(languageContent.copy(content = origContent), ImportStatus.empty)
    result.content should equal(expectedContent)
  }

  test("convert should return a Success with error in importStatus if trying to link to a concept as related content") {
    when(extractConvertStoreContent.processNode(any[String], any[ImportStatus]))
      .thenAnswer((i: InvocationOnMock) => {
        Success(
          (TestData.sampleApiConcept.copy(id = 1),
           i.getArgumentAt(1, ImportStatus.getClass).asInstanceOf[ImportStatus]))
      })
      .thenAnswer((i: InvocationOnMock) => {
        Success(
          (TestData.sampleApiArticle.copy(id = 2),
           i.getArgumentAt(1, ImportStatus.getClass).asInstanceOf[ImportStatus]))
      })

    when(migrationApiClient.getAllTranslationNids(languageContent.nid))
      .thenReturn(Success(Set(languageContent.nid)))

    val Success((result, status)) =
      RelatedContentConverter.convert(languageContent, ImportStatus.empty)

    val expectedContent =
      s"""${languageContent.content}<section><embed data-article-ids="2" data-resource="related-content"></section>"""

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
        Success(
          (TestData.sampleApiArticle.copy(id = 1),
           i.getArgumentAt(1, ImportStatus.getClass).asInstanceOf[ImportStatus]))
      })
      .thenAnswer((i: InvocationOnMock) => {
        Success(
          (TestData.sampleApiArticle.copy(id = 2),
           i.getArgumentAt(1, ImportStatus.getClass).asInstanceOf[ImportStatus]))
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

    when(extractConvertStoreContent.processNode(Matchers.eq("1234"), any[ImportStatus]))
      .thenAnswer((i: InvocationOnMock) => {
        Success(
          (TestData.sampleApiArticle.copy(id = 1),
           i.getArgumentAt(1, ImportStatus.getClass).asInstanceOf[ImportStatus]))
      })
    when(extractConvertStoreContent.processNode(Matchers.eq("5678"), any[ImportStatus]))
      .thenAnswer((i: InvocationOnMock) => {
        Success(
          (TestData.sampleApiArticle.copy(id = 2),
           i.getArgumentAt(1, ImportStatus.getClass).asInstanceOf[ImportStatus]))
      })

    when(migrationApiClient.getAllTranslationNids(languageContent.nid))
      .thenReturn(Success(Set(languageContent.nid)))

    val Success((result, status)) =
      RelatedContentConverter.convert(languageContent, ImportStatus.empty)

    val expectedContent =
      s"""${languageContent.content}<section><embed data-article-ids="2" data-resource="related-content"></section>"""

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

    val expectedContent = origContent + s"""<section><$ResourceHtmlEmbedTag $DataArticleIds="1" $DataResource="$RelatedContent"></section>"""

    val Success((result, _)) =
      RelatedContentConverter.convert(languageContent.copy(content = origContent), ImportStatus.empty)
    result.content should equal(expectedContent)
  }

  test("That convert should only import related content that is in taxonomy") {

    when(taxonomyApiClient.existsInTaxonomy("1234")).thenReturn(Success(false))
    when(taxonomyApiClient.existsInTaxonomy("5678")).thenReturn(Success(true))
    val origContent = "<section><h1>hmm</h1></section>"

    when(extractConvertStoreContent.processNode(any[String], any[ImportStatus]))
      .thenReturn(Success((TestData.sampleApiArticle.copy(id = 1), ImportStatus.empty)))
      .thenReturn(Success((TestData.sampleApiArticle.copy(id = 2), ImportStatus.empty)))

    val expectedContent = origContent + s"""<section><$ResourceHtmlEmbedTag $DataArticleIds="1" $DataResource="$RelatedContent"></section>"""

    val Success((result, _)) =
      RelatedContentConverter.convert(languageContent.copy(content = origContent), ImportStatus.empty)
    result.content should equal(expectedContent)
  }

  test("That convert succeeds with a different error if taxonomy fails") {
    val taxonomyException = new RuntimeException("Taxonomy failed blabla")
    when(taxonomyApiClient.existsInTaxonomy("1234")).thenReturn(Failure(taxonomyException))
    when(taxonomyApiClient.existsInTaxonomy("5678")).thenReturn(Success(true))
    val origContent = "<section><h1>hmm</h1></section>"

    val expectedContent = origContent + s"""<section><$ResourceHtmlEmbedTag $DataArticleIds="2" $DataResource="$RelatedContent"></section>"""

    when(extractConvertStoreContent.processNode(Matchers.eq("1234"), any[ImportStatus]))
      .thenReturn(Success((TestData.sampleApiArticle.copy(id = 1), ImportStatus.empty)))
    when(extractConvertStoreContent.processNode(Matchers.eq("5678"), any[ImportStatus]))
      .thenReturn(Success((TestData.sampleApiArticle.copy(id = 2), ImportStatus.empty)))

    val Success((result, _)) =
      RelatedContentConverter.convert(languageContent.copy(content = origContent), ImportStatus.empty)
    result.content should equal(expectedContent)
  }

}
