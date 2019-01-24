/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service

import no.ndla.articleimport.caching.Memoize
import no.ndla.articleimport.{TestEnvironment, UnitSuite}
import no.ndla.articleimport.integration.{MigrationNodeGeneralContent, TaxonomyApiClient}
import no.ndla.articleimport.model.domain.{ArticleType, ImportStatus}
import org.mockito.Mockito._

import scala.util.{Success, Try}

class ExtractServiceTest extends UnitSuite with TestEnvironment {
  override val extractService = new ExtractService

  val (nodeId1, nodeId2) = ("111", "222")

  val oppgave1 =
    MigrationNodeGeneralContent(nodeId1, nodeId1, "tittel", "oppgave", "nb")

  val oppgave2 =
    MigrationNodeGeneralContent(nodeId2, nodeId1, "tittel", "oppgåve", "nn")

  test("That getNodeOppgave returns all translations of a node when requested node is main node") {
    when(migrationApiClient.getNodeGeneralContent)
      .thenReturn(Memoize[String, Try[Seq[MigrationNodeGeneralContent]]]((id: String) => {
        Try(Map(nodeId1 -> List(oppgave1, oppgave2), nodeId2 -> List(oppgave2))(id))
      }))

    extractService.getNodeGeneralContent(nodeId1) should equal(
      List(oppgave1.asNodeGeneralContent, oppgave2.asNodeGeneralContent))
  }

  test("That getNodeOppgave returns all translations of a node when requested node is a translation") {
    when(migrationApiClient.getNodeGeneralContent)
      .thenReturn(Memoize[String, Try[Seq[MigrationNodeGeneralContent]]]((id: String) => {
        Try(Map(nodeId1 -> List(oppgave1, oppgave2), nodeId2 -> List(oppgave2))(id))
      }))

    extractService.getNodeGeneralContent(nodeId2) should equal(
      List(oppgave1.asNodeGeneralContent, oppgave2.asNodeGeneralContent))
  }

  test("That types are derived correctly from taxonomy") {
    when(taxonomyApiClient.getResource("1111"))
      .thenReturn(Success(Some(TaxonomyApiClient.Resource("", "", Some(""), ""))))
    when(taxonomyApiClient.getTopic("1111")).thenReturn(Success(None))
    when(taxonomyApiClient.getResource("2222")).thenReturn(Success(None))
    when(taxonomyApiClient.getTopic("2222")).thenReturn(Success(None))

    val t1 = extractService.articleTypeFromTaxonomy(Seq("1111", "2222"), ArticleType.Standard, ImportStatus.empty)
    t1._1 should be(ArticleType.Standard)

    val t2 =
      extractService.articleTypeFromTaxonomy(Seq("1111", "2222"), ArticleType.TopicArticle, ImportStatus.empty)
    t2._1 should be(ArticleType.Standard)

    when(taxonomyApiClient.getResource("1111")).thenReturn(Success(None))
    when(taxonomyApiClient.getTopic("1111")).thenReturn(Success(Some(TaxonomyApiClient.Topic("", "", Some(""), ""))))
    when(taxonomyApiClient.getResource("2222")).thenReturn(Success(None))
    when(taxonomyApiClient.getTopic("2222")).thenReturn(Success(None))

    val t3 = extractService.articleTypeFromTaxonomy(Seq("1111", "2222"), ArticleType.Standard, ImportStatus.empty)
    t3._1 should be(ArticleType.TopicArticle)

    val t4 =
      extractService.articleTypeFromTaxonomy(Seq("1111", "2222"), ArticleType.TopicArticle, ImportStatus.empty)
    t4._1 should be(ArticleType.TopicArticle)
  }

  test("Type should be fetched from migration-api if taxonomy api is nonexistant") {
    when(taxonomyApiClient.getResource("1111")).thenReturn(Success(None))
    when(taxonomyApiClient.getTopic("2222")).thenReturn(Success(None))
    when(taxonomyApiClient.getResource("2222")).thenReturn(Success(None))
    when(taxonomyApiClient.getTopic("1111")).thenReturn(Success(None))

    val t1 = extractService.articleTypeFromTaxonomy(Seq("1111", "2222"), ArticleType.Standard, ImportStatus.empty)
    t1._1 should be(ArticleType.Standard)

    val t2 =
      extractService.articleTypeFromTaxonomy(Seq("1111", "2222"), ArticleType.TopicArticle, ImportStatus.empty)
    t2._1 should be(ArticleType.TopicArticle)
  }

  test("Type should be topic-article if exists in taxonomy api, but is inconsistent") {
    when(taxonomyApiClient.getResource("1111"))
      .thenReturn(Success(Some(TaxonomyApiClient.Resource("", "", Some(""), ""))))
    when(taxonomyApiClient.getTopic("2222")).thenReturn(Success(None))
    when(taxonomyApiClient.getResource("2222")).thenReturn(Success(None))
    when(taxonomyApiClient.getTopic("1111")).thenReturn(Success(Some(TaxonomyApiClient.Topic("", "", Some(""), ""))))

    val t1 = extractService.articleTypeFromTaxonomy(Seq("1111", "2222"), ArticleType.Standard, ImportStatus.empty)
    t1._1 should be(ArticleType.TopicArticle)
    t1._2.errors.head.message should be(
      s"Article with nids '1111, 2222' have multiple article types in taxonomy, using type: '${ArticleType.TopicArticle}'.")

    val t2 =
      extractService.articleTypeFromTaxonomy(Seq("1111", "2222"), ArticleType.TopicArticle, ImportStatus.empty)
    t2._1 should be(ArticleType.TopicArticle)
    t2._2.errors.head.message should be(
      s"Article with nids '1111, 2222' have multiple article types in taxonomy, using type: '${ArticleType.TopicArticle}'.")
  }
}
