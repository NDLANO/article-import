/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters.contentbrowser

import no.ndla.articleimport.ArticleImportProperties.Domain
import no.ndla.articleimport.model.domain.ContentFilMeta._
import no.ndla.articleimport.model.domain.{ContentFilMeta, ImportStatus}
import no.ndla.articleimport.{TestData, TestEnvironment, UnitSuite}
import no.ndla.validation.ResourceType
import org.mockito.Mockito._

import scala.util.Success

class FilConverterModuleModuleTest extends UnitSuite with TestEnvironment {
  val nodeId = "1234"
  val title = "melon"

  val content = TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "link_text" -> title)

  override def beforeEach = {
    reset(extractService, attachmentStorageService)
  }

  test("That FilConverter returns an embed to the file") {
    val fileMeta =
      ContentFilMeta(nodeId, "0", "title", "title.pdf", s"$Domain/files/title.pdf", "application/pdf", "1024")
    val filePath = s"$nodeId/${fileMeta.fileName}"
    val expectedResult =
      s"""<FileListEntries data-type="file"><embed data-alt="melon" data-path="files/1234/title.pdf" data-resource="file" data-title="title" data-type="pdf"></FileListEntries>"""

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

  test("FilConverter should return a tentative span with file embeds if more than one file") {
    val filePath = s"$nodeId/test1.pdf"
    val filePath2 = s"$nodeId/test2.pdf"
    val fileMeta =
      ContentFilMeta(nodeId, "0", "title", "title.pdf", s"$Domain/$filePath", "application/pdf", "1024")
    val fileMeta2 = fileMeta.copy(fileName = "title2.pdf", url = s"$Domain/$filePath2")
    val expectedResult =
      s"""$title<FileListEntries data-type="${ResourceType.File.toString}"><embed data-alt="melon" data-path="files/$filePath" data-resource="${ResourceType.File.toString}" data-title="${fileMeta.title}" data-type="pdf"><embed data-alt="melon" data-path="files/$filePath2" data-resource="${ResourceType.File.toString}" data-title="${fileMeta2.title}" data-type="pdf"></FileListEntries>"""

    when(extractService.getNodeFilMeta(nodeId))
      .thenReturn(Success(Seq(fileMeta, fileMeta2)))
    when(attachmentStorageService.uploadFileFromUrl(nodeId, fileMeta))
      .thenReturn(Success(filePath))
    when(attachmentStorageService.uploadFileFromUrl(nodeId, fileMeta2))
      .thenReturn(Success(filePath2))

    val Success((result, _, _)) =
      FilConverterModule.convert(content, ImportStatus.empty)

    result should be(expectedResult)
    verify(extractService, times(1)).getNodeFilMeta(nodeId)
    verify(attachmentStorageService, times(1))
      .uploadFileFromUrl(nodeId, fileMeta)
    verify(attachmentStorageService, times(1))
      .uploadFileFromUrl(nodeId, fileMeta2)
  }
}
