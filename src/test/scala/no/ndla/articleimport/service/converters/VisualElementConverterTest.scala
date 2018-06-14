/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters

import no.ndla.articleimport.model.api.ImportException
import no.ndla.validation.EmbedTagRules.ResourceHtmlEmbedTag
import no.ndla.articleimport.model.domain.ImportStatus
import no.ndla.articleimport.{TestData, TestEnvironment, UnitSuite}
import org.mockito.Mockito._

import scala.util.{Failure, Success}

class VisualElementConverterTest extends UnitSuite with TestEnvironment {
  val nodeId = "1234"
  val sampleArticle = TestData.sampleContent.copy(visualElement = Some(nodeId))

  test("visual element of type image should be converted to embed tag") {
    val expectedResult =
      s"""<$ResourceHtmlEmbedTag data-align="" data-alt="" data-caption="" data-resource="image" data-resource_id="1" data-size="" />"""

    when(extractService.getNodeType(nodeId)).thenReturn(Some("image"))
    when(imageApiClient.importImage(nodeId))
      .thenReturn(Some(TestData.sampleImageMetaInformation))
    val Success((res, _)) =
      VisualElementConverter.convert(sampleArticle, ImportStatus.empty)

    res.visualElement should equal(Some(expectedResult))
  }

  test("Visual element of type image that cannot be found should return a Success with an error message") {
    when(extractService.getNodeType(nodeId)).thenReturn(Some("image"))
    when(imageApiClient.importImage(nodeId)).thenReturn(None)

    val Success((result, status)) = VisualElementConverter.convert(sampleArticle, ImportStatus.empty)

    status.errors should be(Seq(ImportException(nodeId, s"Failed to convert visual element node $nodeId")))
    result.visualElement should be(Some("""<embed data-message="Innhold mangler." data-resource="error" />"""))
  }

  test("visual element of type audio should be converted to embed tag") {
    val expectedResult =
      s"""<$ResourceHtmlEmbedTag data-caption="" data-resource="audio" data-resource_id="1" data-type="standard" />"""

    when(extractService.getNodeType(nodeId)).thenReturn(Some("audio"))
    when(audioApiClient.getOrImportAudio(nodeId)).thenReturn(Success(1: Long))
    val Success((res, _)) =
      VisualElementConverter.convert(sampleArticle, ImportStatus.empty)

    res.visualElement should equal(Some(expectedResult))
  }

  test("Visual element of type audio that cannot be found should return a success with an error message") {
    when(extractService.getNodeType(nodeId)).thenReturn(Some("audio"))
    when(audioApiClient.getOrImportAudio(nodeId))
      .thenReturn(Failure(new RuntimeException()))

    val Success((result, status)) = VisualElementConverter.convert(sampleArticle, ImportStatus.empty)
    result.visualElement should be(Some("""<embed data-message="Innhold mangler." data-resource="error" />"""))
    status.errors should be(Seq(ImportException(nodeId, s"Failed to convert visual element node $nodeId")))
  }

  test("visual element of type video should be converted to embed tag") {
    val expectedResult =
      s"""<$ResourceHtmlEmbedTag data-account="some-account-id" data-caption="" data-player="some-player-id" data-resource="brightcove" data-videoid="ref:1234" />"""

    when(extractService.getNodeType(nodeId)).thenReturn(Some("video"))
    val Success((res, _)) =
      VisualElementConverter.convert(sampleArticle, ImportStatus.empty)
    res.visualElement should equal(Some(expectedResult))
    res.requiredLibraries.size should be(0)
  }

  test("visual element of type h5p should be converted to embed tag") {
    when(h5pApiClient.getViewFromOldId("1234"))
      .thenReturn(Some(s"//ndla.no/h5p/embed/1234"))
    val expectedResult =
      s"""<$ResourceHtmlEmbedTag data-resource="external" data-url="//ndla.no/h5p/embed/1234" />"""

    when(extractService.getNodeType(nodeId)).thenReturn(Some("h5p_content"))
    val Success((res, _)) =
      VisualElementConverter.convert(sampleArticle, ImportStatus.empty)
    res.visualElement should equal(Some(expectedResult))
    res.requiredLibraries.size should be(0)
  }

  test("An empty visual element should return Success without any content modifications") {
    val Success((cont, importStatus)) =
      VisualElementConverter.convert(sampleArticle.copy(visualElement = None), ImportStatus.empty)
    cont should equal(sampleArticle.copy(visualElement = None))
    importStatus should equal(ImportStatus.empty)
  }

  test("If image visual element exists in content, it should be removed") {
    val visId = "6789"
    when(extractService.getNodeType(visId)).thenReturn(Some("image"))
    when(imageApiClient.importImage(visId))
      .thenReturn(Some(TestData.sampleImageMetaInformation.copy(id = visId)))

    val Success((result, _)) = VisualElementConverter.convert(
      sampleArticle.copy(
        content =
          s"""<$ResourceHtmlEmbedTag data-align="" data-alt="" data-caption="" data-resource="image" data-resource_id="$visId" data-size="" />${sampleArticle.content}""",
        visualElement = Some(visId)
      ),
      ImportStatus.empty
    )
    result.content should equal(sampleArticle.content)
  }

  test("If video visual element exists in content, it should be removed") {
    val visId = "6789"
    when(extractService.getNodeType(visId)).thenReturn(Some("video"))
    val Success((result, _)) = VisualElementConverter.convert(
      sampleArticle.copy(
        content =
          s"""<$ResourceHtmlEmbedTag data-account="some-account-id" data-caption="" data-player="some-player-id" data-resource="brightcove" data-videoid="ref:$visId" />${sampleArticle.content}""",
        visualElement = Some(visId)
      ),
      ImportStatus.empty
    )
    result.content should equal(sampleArticle.content)
  }

  test("If h5p visual element exists in content, it should be removed") {
    val visId = "6789"
    when(h5pApiClient.getViewFromOldId("6789"))
      .thenReturn(Some(s"//ndla.no/h5p/embed/$visId"))
    when(extractService.getNodeType(visId)).thenReturn(Some("h5p_content"))
    val Success((result, _)) = VisualElementConverter.convert(
      sampleArticle.copy(
        content =
          s"""<$ResourceHtmlEmbedTag data-resource="external" data-url="//ndla.no/h5p/embed/$visId" />${sampleArticle.content}""",
        visualElement = Some(visId)
      ),
      ImportStatus.empty
    )
    result.content should equal(sampleArticle.content)
  }

  test("If audio visual element exists in content, it should be removed") {
    val visId = "6789"
    when(extractService.getNodeType(visId)).thenReturn(Some("audio"))
    when(audioApiClient.getOrImportAudio(visId))
      .thenReturn(Success(visId.toLong: Long))

    val Success((result, _)) = VisualElementConverter.convert(
      sampleArticle.copy(
        content =
          s"""<$ResourceHtmlEmbedTag data-caption="" data-resource="audio" data-resource_id="$visId" data-type="standard" />${sampleArticle.content}""",
        visualElement = Some(visId)
      ),
      ImportStatus.empty
    )
    result.content should equal(sampleArticle.content)
  }

}
