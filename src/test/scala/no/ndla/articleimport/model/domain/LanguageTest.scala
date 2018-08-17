/*
 * Part of NDLA article-import.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.model.domain
import no.ndla.articleimport.{TestEnvironment, UnitSuite}

class LanguageTest extends UnitSuite with TestEnvironment {
  test("findByLanguageOrBestEffort should return language selected or most prioritized") {
    val nbTitle = ArticleTitle(title = "Hei", "nb")
    val esTitle = ArticleTitle(title = "Hola", "es")
    val enTitle = ArticleTitle(title = "Hey", "en")
    val unknown = ArticleTitle(title = "!!", "unknown")

    // Return correct title
    Language.findByLanguageOrBestEffort(List(esTitle, nbTitle, enTitle), "nb").get should be(nbTitle)
    Language.findByLanguageOrBestEffort(List(nbTitle, enTitle, esTitle), "es").get should be(esTitle)
    Language.findByLanguageOrBestEffort(List(nbTitle, enTitle, esTitle), "en").get should be(enTitle)

    // Returns pritorized title
    Language.findByLanguageOrBestEffort(List(nbTitle, enTitle, esTitle), "zh").get should be(nbTitle)
    Language.findByLanguageOrBestEffort(List(esTitle, enTitle), "nb").get should be(enTitle)
    Language.findByLanguageOrBestEffort(List(esTitle, enTitle, unknown), "nb").get should be(unknown)
  }
}
