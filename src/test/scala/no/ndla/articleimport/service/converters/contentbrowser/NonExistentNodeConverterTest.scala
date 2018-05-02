/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters.contentbrowser

import no.ndla.articleimport.model.domain.ImportStatus
import no.ndla.articleimport.{TestData, TestEnvironment, UnitSuite}

class NonExistentNodeConverterTest extends UnitSuite with TestEnvironment {
  val nodeId = "1234"
  val content = TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId)

  test("That NonExistentNodeConverter returns a Failure") {
    NonExistentNodeConverter
      .convert(content, ImportStatus.empty)
      .isFailure should be(true)
  }
}
