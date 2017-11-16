/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.articleimport.service

import no.ndla.articleimport.{TestEnvironment, UnitSuite}
import no.ndla.articleimport.integration.MigrationNodeGeneralContent
import org.mockito.Mockito._

import scala.util.Success

class ExtractServiceTest extends UnitSuite with TestEnvironment {
  override val extractService = new ExtractService

  val (nodeId1, nodeId2) = ("111", "222")
  val oppgave1 = MigrationNodeGeneralContent(nodeId1, nodeId1, "tittel", "oppgave", "nb")
  val oppgave2 = MigrationNodeGeneralContent(nodeId2, nodeId1, "tittel", "oppgåve", "nn")

  test("That getNodeOppgave returns all translations of a node when requested node is main node") {
    when(migrationApiClient.getNodeGeneralContent(nodeId1)).thenReturn(Success(List(oppgave1, oppgave2)))
    when(migrationApiClient.getNodeGeneralContent(nodeId2)).thenReturn(Success(List(oppgave2)))

    extractService.getNodeGeneralContent(nodeId1) should equal (List(oppgave1.asNodeGeneralContent, oppgave2.asNodeGeneralContent))
  }

  test("That getNodeOppgave returns all translations of a node when requested node is a translation") {
    when(migrationApiClient.getNodeGeneralContent(nodeId1)).thenReturn(Success(List(oppgave1, oppgave2)))
    when(migrationApiClient.getNodeGeneralContent(nodeId2)).thenReturn(Success(List(oppgave2)))

    extractService.getNodeGeneralContent(nodeId2) should equal (List(oppgave1.asNodeGeneralContent, oppgave2.asNodeGeneralContent))
  }
}
