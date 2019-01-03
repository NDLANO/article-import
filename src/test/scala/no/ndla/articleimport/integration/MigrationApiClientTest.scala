/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.integration

import no.ndla.articleimport.{TestEnvironment, UnitSuite}
import org.joda.time.DateTime
import org.junit.Test

class MigrationApiClientTest extends UnitSuite with TestEnvironment {
  val migrationIngress = MigrationIngress("123", Option("ingress from  table"), None, 1, Option("nb"))

  val migrationContent = MigrationContent("124",
                                          "124",
                                          "content",
                                          "metadescription",
                                          Option("nb"),
                                          DateTime.now().toDate,
                                          DateTime.now().toDate)

  val emneArtikkelData =
    MigrationEmneArtikkelData("ingress from emneartikkel", "metadescription from emneartikkel", Option("nb"))

  val migrationMainNodeImport = MigrationMainNodeImport(
    Seq(),
    Seq(migrationIngress),
    Seq(migrationContent),
    Seq(),
    Option("by-sa"),
    Option("emneartikkel"),
    Seq(),
    Seq(),
    Seq(),
    Seq(),
    Seq(),
    Seq(),
    Seq(),
    Seq(),
    Seq(),
    Seq(emneArtikkelData)
  )

  test("asLanguageContents uses ingress from emneartikkel if emneartikkeldata is present") {
    migrationMainNodeImport.asLanguageContents.head.ingress.get should equal(
      LanguageIngress(emneArtikkelData.ingress, None))
  }

  test("asLanguageContents uses ingress from separate ingress field if present") {
    migrationMainNodeImport
      .copy(emneartikkelData = Seq())
      .asLanguageContents
      .head
      .ingress
      .get should equal(LanguageIngress(migrationIngress.content.get, migrationIngress.imageNid))
  }

  test("asLanguageContents uses metaDescription as metadescription from emneartikkel if present") {
    migrationMainNodeImport.asLanguageContents.head.metaDescription should equal(emneArtikkelData.metaDescription)
  }

  test("asLanguageContents uses ingress as metadescription from content if emneartikkel is not present") {
    migrationMainNodeImport
      .copy(emneartikkelData = Seq.empty)
      .asLanguageContents
      .head
      .metaDescription should equal(migrationIngress.content.get)
  }

  test("asLanguageContents only uses visual element if TopicArticle") {
    val node = migrationMainNodeImport
      .copy(nodeType = None)
      .asNodeToConvert("", List())

    node.contents.map(_.visualElement) should be(List(None))

    val node2 = migrationMainNodeImport
      .copy(nodeType = Some("random value"))
      .asNodeToConvert("", List())

    node2.contents.map(_.visualElement) should be(List(None))

    val node3 = migrationMainNodeImport
      .copy(visualElements = Seq(MigrationVisualElement("Visual", "Element", Some("nb"))))
      .asNodeToConvert("", List())

    node3.contents.map(_.visualElement) should be(List(Some("Visual")))
  }
}
