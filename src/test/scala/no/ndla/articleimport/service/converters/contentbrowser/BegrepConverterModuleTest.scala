/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters.contentbrowser

import no.ndla.articleimport.model.domain.ImportStatus
import no.ndla.articleimport.{TestData, TestEnvironment, UnitSuite}
import no.ndla.validation.EmbedTagRules.ResourceHtmlEmbedTag
import no.ndla.articleimport.model.api.ImportException
import no.ndla.validation.ValidationException
import org.mockito.Mockito._

import scala.util.{Failure, Success}

class BegrepConverterModuleTest extends UnitSuite with TestEnvironment {
  val nodeId = "1234"
  val linkText = "begrepsnoder"
  val contentString = s"[contentbrowser ==nid=$nodeId==imagecache=Fullbredde==width===alt===link===node_link=1==link_type=link_to_content==lightbox_size===remove_fields[76661]=1==remove_fields[76663]=1==remove_fields[76664]=1==remove_fields[76666]=1==insertion===link_title_text= ==link_text=$linkText==text_align===css_class=contentbrowser contentbrowser]"
  val content = ContentBrowser(contentString, "nb")

  test("begrep should be imported and inserted as an embed tag in the article") {
    when(extractConvertStoreContent.processNode(nodeId, ImportStatus.empty)).thenReturn(Success((TestData.sampleApiConcept, ImportStatus.empty)))

    val expectedResult = s"""<$ResourceHtmlEmbedTag data-content-id="1" data-link-text="$linkText" data-resource="concept" />"""
    val Success((result, requiredLibs, _)) = BegrepConverter.convert(content, ImportStatus.empty)

    requiredLibs.isEmpty should be (true)
    result should equal(expectedResult)
  }

  test("begrepconverter should return a failure if node is not a begrep") {
    when(extractConvertStoreContent.processNode(nodeId, ImportStatus.empty)).thenReturn(Success((TestData.sampleApiArticle, ImportStatus.empty)))
    val result = BegrepConverter.convert(content, ImportStatus.empty)

    result.isFailure should be (true)
    val exceptionMsg = result.failed.get.asInstanceOf[ImportException].message
    exceptionMsg.startsWith("THIS IS A BUG") should be (true)
  }

  test("begrepconverter should return a failure if node failed to be imported") {
    when(extractConvertStoreContent.processNode(nodeId, ImportStatus.empty)).thenReturn(Failure(new ValidationException(errors=Seq.empty)))
    val result = BegrepConverter.convert(content, ImportStatus.empty)

    result.isFailure should be (true)
  }
}
