/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.domain

import no.ndla.articleimport.model.domain.emptySomeToNone
import no.ndla.articleimport.{TestEnvironment, UnitSuite}

class SafeLanguageStringTest extends UnitSuite with TestEnvironment {

  test("emtpySomeToNone should return None on Some(\"\")") {
    emptySomeToNone(Some("")) should equal(None)
  }
  test("emtpySomeToNone should return Some with same content on non empty") {
    emptySomeToNone(Some("I have content :)")) should equal(Some("I have content :)"))
  }
  test("emtpySomeToNone should return None on None") {
    emptySomeToNone(None) should equal(None)
  }

}
