/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.articleimport.service.converters

import no.ndla.articleimport.model.domain.ImportStatus
import no.ndla.articleimport.{TestData, TestEnvironment, UnitSuite}
import no.ndla.validation.EmbedTagRules.ResourceHtmlEmbedTag

import scala.util.Success

class LeafNodeConverterTest extends UnitSuite with TestEnvironment {
  val nodeId = "1234"

  test("Leaf node converter should create an article from a pure video node") {
    val sampleLanguageContent = TestData.sampleContent.copy(content="<p>lolol</p>", nodeType = "video")
    val expectedResult = s"""<section><$ResourceHtmlEmbedTag data-account="some-account-id" data-caption="" data-player="some-player-id" data-resource="brightcove" data-videoid="ref:${sampleLanguageContent.nid}"></section>${sampleLanguageContent.content}"""
    val Success((result, _)) = LeafNodeConverter.convert(sampleLanguageContent, ImportStatus.empty)

    result.content should equal (expectedResult)
    result.requiredLibraries.size should equal (1)
  }

  test("Leaf node converter should create an article from a pure h5p node") {
    val sampleLanguageContent = TestData.sampleContent.copy(content="<div><h1>hi</h1></div>", nodeType = "h5p_content")
    val expectedResult = s"""<section><$ResourceHtmlEmbedTag data-resource="h5p" data-url="//ndla.no/h5p/embed/1234"></section>${sampleLanguageContent.content}"""
    val Success((result, _)) = LeafNodeConverter.convert(sampleLanguageContent, ImportStatus.empty)

    result.content should equal (expectedResult)
    result.requiredLibraries.size should equal (1)
  }

}
