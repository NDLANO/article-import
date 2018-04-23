/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters.contentbrowser

import no.ndla.articleimport.model.domain.ImportStatus
import no.ndla.articleimport.{TestEnvironment, UnitSuite}

class NonExistentNodeConverterTest extends UnitSuite with TestEnvironment {
  val nodeId = "1234"

  val contentString =
    s"[contentbrowser ==nid=$nodeId==imagecache=Fullbredde==width===alt=Melon==link===node_link=1==link_type=link_to_content==lightbox_size===remove_fields[76661]=1==remove_fields[76663]=1==remove_fields[76664]=1==remove_fields[76666]=1==insertion===link_title_text= ==link_text= ==text_align===css_class=contentbrowser contentbrowser]"

  test("That NonExistentNodeConverter returns a Failure") {
    val content = ContentBrowserString(contentString, "nb")
    NonExistentNodeConverter
      .convert(content, ImportStatus.empty)
      .isFailure should be(true)
  }
}
