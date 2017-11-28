/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.articleimport.controller

import java.util.Date

import no.ndla.articleimport.TestData._
import no.ndla.articleimport.model.domain._
import no.ndla.articleimport.{TestData, TestEnvironment, UnitSuite}
import org.json4s.native.Serialization._
import org.mockito.Mockito._
import org.scalatra.test.scalatest.ScalatraFunSuite

import scala.util.{Failure, Try}

class InternControllerTest extends UnitSuite with TestEnvironment with ScalatraFunSuite {
  implicit val formats = org.json4s.DefaultFormats

  val author = Author("forfatter", "Henrik")
  val sampleNode = NodeToConvert(List(sampleTitle), List(sampleContent), "by-sa", Seq(author), List(ArticleTag(List("tag"), "en")), "fagstoff", "fagstoff", new Date(0), new Date(1), ArticleType.Standard)
  val sampleNode2 = sampleNode.copy(contents = List(sampleTranslationContent))
  lazy val controller = new InternController
  addServlet(controller, "/*")

  test("That POST /import/:node_id returns 500 if the main node is not found") {

    when(extractService.getNodeData(nodeId2)).thenReturn(sampleNode2)

    post(s"/import/$nodeId2") {
      status should equal(500)
    }
  }

  test("That POST /import/:node_id returns a json status-object on success") {
    val newNodeId: Long = 4444
    val newArticle = TestData.sampleApiArticle.copy(id=newNodeId)
    when(extractConvertStoreContent.processNode(nodeId, false)).thenReturn(Try((newArticle, ImportStatus.empty)))

    post(s"/import/$nodeId", "forceUpdate" -> "false") {
      status should equal(200)
      val convertedBody = read[ImportStatus](body)
      convertedBody should equal(ImportStatus(s"Successfully imported node $nodeId: $newNodeId", Set[String]()))
    }
  }

  test("That POST /import/:node_id status code is 500 with a message if processNode fails") {
    when(extractConvertStoreContent.processNode(nodeId, false)).thenReturn(Failure(new RuntimeException("processNode failed")))

    post(s"/import/$nodeId", "forceUpdate" -> "false") {
      status should equal(500)
    }
  }

}
