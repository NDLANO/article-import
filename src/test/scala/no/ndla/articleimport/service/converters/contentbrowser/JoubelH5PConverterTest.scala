/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.articleimport.service.converters.contentbrowser

import no.ndla.articleimport.{TestEnvironment, UnitSuite}
import no.ndla.validation.EmbedTagRules.ResourceHtmlEmbedTag
import no.ndla.articleimport.model.domain.ImportStatus

import scala.util.Success

class JoubelH5PConverterTest extends UnitSuite with TestEnvironment {
  val (validNodeId, joubelH5PIdId) = ValidH5PNodeIds.head
  val invalidNodeId = "123123"
  val altText = "Jente som spiser melom. Grønn bakgrunn, rød melon. Fotografi."
  val contentStringWithValidNodeId = s"[contentbrowser ==nid=$validNodeId==imagecache=Fullbredde==width===alt=$altText==link===node_link=1==link_type=link_to_content==lightbox_size===remove_fields[76661]=1==remove_fields[76663]=1==remove_fields[76664]=1==remove_fields[76666]=1==insertion===link_title_text= ==link_text= ==text_align===css_class=contentbrowser contentbrowser]"
  val contentStringWithInvalidNodeId = s"[contentbrowser ==nid=$invalidNodeId==imagecache=Fullbredde==width===alt=$altText==link===node_link=1==link_type=link_to_content==lightbox_size===remove_fields[76661]=1==remove_fields[76663]=1==remove_fields[76664]=1==remove_fields[76666]=1==insertion===link_title_text= ==link_text= ==text_align===css_class=contentbrowser contentbrowser]"

  test("Contentbrowser strings of type 'h5p_content' returns a h5p embed-resource") {
    val expectedResult = s"""<$ResourceHtmlEmbedTag data-resource="h5p" data-url="${JoubelH5PConverter.JoubelH5PBaseUrl}/$joubelH5PIdId" />"""
    val content = ContentBrowser(contentStringWithValidNodeId, "nb")
    val Success((result, requiredLibraries, errors)) = JoubelH5PConverter.convert(content, ImportStatus.empty)

    result should equal(expectedResult)
    errors.messages.length should equal(0)
    requiredLibraries.length should equal (0)
  }

  test("H5P nodes with an invalid node id returns an error embed-resource") {
    val expectedResult = s"""<$ResourceHtmlEmbedTag data-message="H5P node $invalidNodeId is not yet exported to new H5P service" data-resource="error" />"""
    val content = ContentBrowser(contentStringWithInvalidNodeId, "nb")
    val Success((result, requiredLibraries, errors)) = JoubelH5PConverter.convert(content, ImportStatus.empty)

    result should equal(expectedResult)
    errors.messages.length should equal(1)
    requiredLibraries.length should equal (0)
  }
}
