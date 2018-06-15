/*
 * Part of NDLA article_import.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters

import no.ndla.articleimport.model.domain.{ContentFilMeta, ImportStatus}
import no.ndla.articleimport.{TestData, TestEnvironment, UnitSuite}
import no.ndla.validation.ResourceType
import no.ndla.articleimport.ArticleImportProperties.Domain
import no.ndla.articleimport.model.domain.ContentFilMeta._
import org.mockito.Mockito._

import scala.util.Success

class FileDivConverterTest extends UnitSuite with TestEnvironment {
  private val nodeId = "1234"
  private val defaultImportStatus = ImportStatus.empty

  test("A span file embed should be moved to bottom of p-tag and be converted to div") {
    val fileMeta =
      ContentFilMeta(nodeId, "0", "title", "title.pdf", s"$Domain/files/title.pdf", "application/pdf", "1024")
    val fileMeta2 = fileMeta.copy(fileName = "title2.pdf", url = s"$Domain/files/title2.pdf")
    val filePath = s"$nodeId/${fileMeta.fileName}"
    val filePath2 = s"$nodeId/${fileMeta2.fileName}"

    val embeds =
      s"""<embed data-resource="${ResourceType.File.toString}" data-title="${fileMeta.title}" data-type="pdf" data-url="$filePath">
         |<embed data-resource="${ResourceType.File.toString}" data-title="${fileMeta2.title}" data-type="pdf" data-url="$filePath2">""".stripMargin

    val originalContent =
      s"""<section><h1>Article here</h1><p>Files for this can be found on this page: "melon<FileListEntries data-type="${ResourceType.File.toString}">"$embeds</FileListEntries>" so please visit it :D</p></section>"""
    val expectedContent =
      s"""<section><h1>Article here</h1><p>Files for this can be found on this page: "melon" so please visit it :D</p><div data-type="${ResourceType.File.toString}">"$embeds</div></section>"""

    val content = TestData.sampleContent.copy(content = originalContent)
    val Success((result, _)) =
      FileDivConverter.convert(content, defaultImportStatus)

    result.content should equal(expectedContent)

  }
}
