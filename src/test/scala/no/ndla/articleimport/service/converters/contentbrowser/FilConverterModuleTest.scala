/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters.contentbrowser

import no.ndla.articleimport.model.domain.{ContentFilMeta, ImportStatus}
import no.ndla.articleimport.{TestData, TestEnvironment, UnitSuite}
import org.mockito.Mockito._
import no.ndla.articleimport.model.domain.ContentFilMeta._
import no.ndla.articleimport.ArticleImportProperties.Domain
import no.ndla.articleimport.model.api.ImportException

import scala.util.{Failure, Success}

class FilConverterModuleTest extends UnitSuite with TestEnvironment {
  val nodeId = "1234"
  val title = "melon"

  val content = TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "link_text" -> title)

  override def beforeEach = {
    reset(extractService, attachmentStorageService)
  }

  test("That FilConverter returns a link to the file") {
    val fileMeta =
      ContentFilMeta(nodeId, "0", "title", "title.pdf", s"$Domain/files/title.pdf", "application/pdf", "1024")
    val filePath = s"$nodeId/${fileMeta.fileName}"
    val expectedResult =
      s"""<a href="$Domain/files/$filePath" title="${fileMeta.fileName}">${fileMeta.fileName}</a>"""

    when(extractService.getNodeFilMeta(nodeId))
      .thenReturn(Success(Seq(fileMeta)))
    when(attachmentStorageService.uploadFileFromUrl(nodeId, fileMeta))
      .thenReturn(Success(filePath))
    val Success((result, _, _)) =
      FilConverterModule.convert(content, ImportStatus.empty)

    result should equal(expectedResult)
    verify(extractService, times(1)).getNodeFilMeta(nodeId)
    verify(attachmentStorageService, times(1))
      .uploadFileFromUrl(nodeId, fileMeta)
  }

  test("FilConverter should return a Failure if file node contains more than one file") {
    val fileMeta =
      ContentFilMeta(nodeId, "0", "title", "title.pdf", s"$Domain/files/title.pdf", "application/pdf", "1024")
    val filePath = s"$nodeId/${fileMeta.fileName}"
    val expectedResult =
      s"""<a href="$Domain/files/$filePath" title="${fileMeta.fileName}">${fileMeta.fileName}</a>"""

    when(extractService.getNodeFilMeta(nodeId))
      .thenReturn(Success(Seq(fileMeta, fileMeta.copy(fileName = "title2.pdf"))))
    val Failure(result: ImportException) =
      FilConverterModule.convert(content, ImportStatus.empty)

    result.getMessage.contains("File node contains more than one file") should be(true)
    verify(extractService, times(1)).getNodeFilMeta(nodeId)
    verify(attachmentStorageService, times(0))
      .uploadFileFromUrl(nodeId, fileMeta)
  }
}
