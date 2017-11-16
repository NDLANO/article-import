/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.articleimport.service.converters.contentbrowser

import no.ndla.articleimport.model.domain.{ContentFilMeta, ImportStatus}
import no.ndla.articleimport.{TestEnvironment, UnitSuite}
import org.mockito.Mockito._
import no.ndla.articleimport.model.domain.ContentFilMeta._
import no.ndla.articleimport.ArticleImportProperties.Domain

import scala.util.Success

class FilConverterTest extends UnitSuite with TestEnvironment {
  val nodeId = "1234"
  val title = "melon"
  val contentString = s"[contentbrowser ==nid=$nodeId==imagecache=Fullbredde==width===alt=totoggram==link===node_link=1==link_type=link_to_content==lightbox_size===remove_fields[76661]=1==remove_fields[76663]=1==remove_fields[76664]=1==remove_fields[76666]=1==insertion===link_title_text= ==link_text=$title==text_align===css_class=contentbrowser contentbrowser]"

  test("That FilConverter returns a link to the file") {
    val content = ContentBrowser(contentString, "nb")
    val fileMeta = ContentFilMeta(nodeId, "0", "title", "title.pdf", s"$Domain/files/title.pdf", "application/pdf", "1024")
    val filePath = s"$nodeId/${fileMeta.fileName}"
    val expectedResult = s"""<a href="$Domain/files/$filePath" title="${fileMeta.fileName}">${fileMeta.fileName}</a>"""

    when(extractService.getNodeFilMeta(nodeId)).thenReturn(Success(fileMeta))
    when(attachmentStorageService.uploadFileFromUrl(nodeId, fileMeta)).thenReturn(Success(filePath))
    val Success((result, _, _)) = FilConverter.convert(content, ImportStatus.empty)

    result should equal(expectedResult)
    verify(extractService, times(1)).getNodeFilMeta(nodeId)
    verify(attachmentStorageService, times(1)).uploadFileFromUrl(nodeId, fileMeta)
  }
}
