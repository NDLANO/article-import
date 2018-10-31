/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters

import no.ndla.articleimport.integration.MigrationEmbedMeta
import no.ndla.articleimport.model.api.ImportException
import no.ndla.articleimport.model.domain.ImportStatus
import no.ndla.articleimport.{TestData, TestEnvironment, UnitSuite}
import no.ndla.validation.EmbedTagRules.ResourceHtmlEmbedTag
import no.ndla.validation.ResourceType
import org.mockito.Mockito._
import org.mockito.ArgumentMatchers._

import scala.util.{Failure, Success}

class LeafNodeConverterTest extends UnitSuite with TestEnvironment {
  val nodeId = "1234"

  test("Leaf node converter should create an article from a pure video node") {
    val sampleLanguageContent =
      TestData.sampleContent.copy(content = "<p>lolol</p>", nodeType = "video")
    val expectedResult =
      s"""<section><$ResourceHtmlEmbedTag data-account="some-account-id" data-caption="" data-player="some-player-id" data-resource="brightcove" data-videoid="ref:${sampleLanguageContent.nid}"></section>${sampleLanguageContent.content}"""
    val Success((result, _)) =
      LeafNodeConverter.convert(sampleLanguageContent, ImportStatus.empty)

    result.content should equal(expectedResult)
    result.metaDescription should equal("Beskrivelse mangler")
    result.requiredLibraries.size should equal(0)
  }

  test("Leaf node converter should create an article from a pure h5p node") {
    when(h5pApiClient.getViewFromOldId("1234"))
      .thenReturn(Some(s"//ndla.no/h5p/embed/1234"))
    val sampleLanguageContent = TestData.sampleContent
      .copy(content = "<div><h1>hi</h1></div>", nodeType = "h5p_content")
    val expectedResult =
      s"""${sampleLanguageContent.content}<section><$ResourceHtmlEmbedTag data-resource="external" data-url="//ndla.no/h5p/embed/1234"></section>"""
    val Success((result, _)) =
      LeafNodeConverter.convert(sampleLanguageContent, ImportStatus.empty)

    result.content should equal(expectedResult)
    result.metaDescription should equal("metadescription")
    result.requiredLibraries.size should equal(0)
  }

  test("Leaf node converter should create an article from a pure lenkenode") {
    val nid = "1234"
    val sampleLanguageContent = TestData.sampleContent.copy(nid = nid, nodeType = "lenke")
    val KahootUrl = "https://play.kahoot.it/#/k/e577f7e9-59ff-4a80-89a1-c95acf04815d"
    val KahootSrc = "https://embed.kahoot.it/e577f7e9-59ff-4a80-89a1-c95acf04815d"
    val KahootEmbedCode =
      s"""<iframe src="$KahootSrc" name="iframe1" scrolling="no" frameborder="no" align="center" height = "350px" width = "620px"></iframe>"""
    val expectedResult =
      s"""<$ResourceHtmlEmbedTag data-height="350px" data-resource="${ResourceType.IframeContent}" data-url="$KahootSrc" data-width="620px" />"""

    val embedMeta = Success(MigrationEmbedMeta(Some(KahootUrl), Some(KahootEmbedCode)))
    when(extractService.getLinkEmbedMeta(any[String])).thenReturn(embedMeta)
    val Success((result, _)) = LeafNodeConverter.convert(sampleLanguageContent, ImportStatus.empty)

    result.content should be(expectedResult)
    result.metaDescription should be(KahootUrl)
  }

  test("Leaf node converter should fail if lenkenode url is unsupported") {
    val nid = "1234"
    val sampleLanguageContent = TestData.sampleContent.copy(nid = nid, nodeType = "lenke")
    val UnsupportedUrl = "https://play.unsupporteddomain.com/#/k/e577f7e9-59ff-4a80-89a1-c95acf04815d"
    val UnsupportedSrc = "https://embed.unsupporteddomain.com/e577f7e9-59ff-4a80-89a1-c95acf04815d"
    val UnsupportedEmbedCode =
      s"""<iframe src="$UnsupportedSrc" name="iframe1" scrolling="no" frameborder="no" align="center" height = "350px" width = "620px"></iframe>"""

    val embedMeta = Success(MigrationEmbedMeta(Some(UnsupportedUrl), Some(UnsupportedEmbedCode)))
    when(extractService.getLinkEmbedMeta(any[String])).thenReturn(embedMeta)
    val result = LeafNodeConverter.convert(sampleLanguageContent, ImportStatus.empty)

    result.isFailure should be(true)
    val Failure(exception: ImportException) = result
    exception.message should be(s"'$UnsupportedUrl' is not a whitelisted embed source")
    exception.nid should be(nid)
  }

  test("LeafNodeConverter should succeed with error if h5p fails") {
    when(h5pApiClient.getViewFromOldId("1234"))
      .thenReturn(None)
    val sampleLanguageContent = TestData.sampleContent
      .copy(content = "<div><h1>hi</h1></div>", nodeType = "h5p_content")
    val expectedResult =
      s"""${sampleLanguageContent.content}"""
    val Success((result, status)) =
      LeafNodeConverter.convert(sampleLanguageContent, ImportStatus.empty)

    result.content should equal(expectedResult)
    result.metaDescription should equal("metadescription")
    result.requiredLibraries.size should equal(0)
    status.errors should be(
      Seq(ImportException("1234", "Failed to import H5P with id 1234: Not yet exported to new H5P service")))
  }

}
