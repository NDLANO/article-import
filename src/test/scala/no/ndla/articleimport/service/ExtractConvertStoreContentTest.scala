/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service

import java.util.Date

import no.ndla.articleimport.integration.{ContentId, LanguageIngress, MigrationSubjectMeta}
import no.ndla.articleimport.model.api.{ArticleStatus, OptimisticLockException}
import no.ndla.articleimport.model.domain._
import no.ndla.articleimport.{TestData, TestEnvironment, UnitSuite}
import no.ndla.network.model.HttpRequestException
import no.ndla.validation.{ValidationException, ValidationMessage}
import org.mockito.{ArgumentMatcher, Matchers}
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.mockito.invocation.InvocationOnMock
import org.scalatest.matchers.{MatchResult, Matcher}

import scala.util.{Failure, Success, Try}

class ExtractConvertStoreContentTest extends UnitSuite with TestEnvironment {
  override val converterService = new ConverterService
  val (nodeId, nodeId2) = ("1234", "4321")
  val newNodeid: Long = 4444
  val sampleTitle = ArticleTitle("title", "en")
  val sampleIngress = LanguageIngress("ingress here", None)

  val contentString =
    s"[contentbrowser ==nid=$nodeId2==imagecache=Fullbredde==width===alt=alttext==link===node_link=1==link_type=link_to_content==lightbox_size===remove_fields[76661]=1==remove_fields[76663]=1==remove_fields[76664]=1==remove_fields[76666]=1==insertion=link==link_title_text===link_text=Tittel==text_align===css_class=contentbrowser contentbrowser]"
  val sampleContent = TestData.sampleContent.copy(content = contentString)
  val author = Author("forfatter", "Henrik")

  val sampleNode = NodeToConvert(
    List(sampleTitle),
    List(sampleContent),
    "by-sa",
    Seq(author),
    List(ArticleTag(List("tag"), "en")),
    "fagstoff",
    "fagstoff",
    new Date(0),
    new Date(1),
    ArticleType.Standard,
    Seq.empty
  )

  val eCSService = new ExtractConvertStoreContent

  override def beforeEach: Unit = {
    when(extractService.getNodeData(nodeId)).thenReturn(Success(sampleNode))
    when(extractService.getNodeType(nodeId2)).thenReturn(Some("fagstoff"))
    when(extractService.getNodeGeneralContent(nodeId2))
      .thenReturn(Seq(NodeGeneralContent(nodeId2, nodeId2, "title", "content", "en")))
    when(draftApiClient.getArticleIdFromExternalId(nodeId)).thenReturn(None)
    when(draftApiClient.getArticleIdFromExternalId(nodeId2)).thenReturn(None)
    when(migrationApiClient.getSubjectForNode(nodeId))
      .thenReturn(Try(Set(MigrationSubjectMeta("52", "helsearbeider vg2"))))

    when(draftApiClient.getArticleIdFromExternalId(sampleNode.contents.head.nid))
      .thenReturn(Some(1: Long))
    when(draftApiClient.newEmptyArticle(any[String], any[Set[String]]))
      .thenReturn(Success(ContentId(TestData.sampleArticleWithPublicDomain.id.get)))
    when(extractConvertStoreContent.processNode("9876"))
      .thenReturn(Try(TestData.sampleApiArticle, ImportStatus.empty))

    when(extractConvertStoreContent.getMainNodeId(any[String]))
      .thenAnswer((invocation: InvocationOnMock) => Some(invocation.getArgumentAt(0, classOf[String])))
  }

  test("That ETL extracts, translates and loads a node correctly") {
    val sampleArticle = TestData.sampleApiArticle
    when(extractConvertStoreContent.processNode(nodeId2, ImportStatus(Seq(), Set(nodeId))))
      .thenReturn(Try((sampleArticle, ImportStatus(Seq(), Set(nodeId, nodeId2)))))
    when(draftApiClient.getConceptIdFromExternalId(any[String]))
      .thenReturn(None)
    when(draftApiClient.getArticleIdFromExternalId(any[String]))
      .thenReturn(None)
    when(draftApiClient.newArticle(any[Article], any[String], any[Set[String]]))
      .thenReturn(Success(TestData.sampleApiArticle))
    when(draftApiClient.publishArticle(any[Long]))
      .thenReturn(Success(ArticleStatus(Set("IMPORTED", "PUBLISHED"))))

    val Success((_, status)) = eCSService.processNode(nodeId)
    status should equal(
      ImportStatus(List(s"Successfully imported node $nodeId: 1"), Set(nodeId, nodeId2), Some(sampleArticle.id), false))
    verify(draftApiClient, times(1))
      .newArticle(any[Article], any[String], any[Set[String]])
  }

  test("That ETL returns a list of visited nodes") {
    val sampleArticle = TestData.sampleApiArticle
    when(
      extractConvertStoreContent
        .processNode(nodeId2, ImportStatus(Seq(), Set("9876", nodeId))))
      .thenReturn(Try((sampleArticle, ImportStatus(Seq(), Set("9876", nodeId, nodeId2)))))
    when(draftApiClient.getConceptIdFromExternalId(any[String]))
      .thenReturn(Some(1: Long))
    when(draftApiClient.getArticleIdFromExternalId(any[String]))
      .thenReturn(None)
    when(draftApiClient.newArticle(any[Article], any[String], any[Set[String]]))
      .thenReturn(Success(TestData.sampleApiArticle))
    when(draftApiClient.publishArticle(any[Long]))
      .thenReturn(Success(ArticleStatus(Set("IMPORTED", "PUBLISHED"))))

    val Success((_, status)) =
      eCSService.processNode(nodeId, ImportStatus(Seq(), Set("9876")))
    status should equal(
      ImportStatus(List(s"Successfully imported node $nodeId: 1"), Set("9876", nodeId), Some(sampleArticle.id), false))
  }

  test("That ETL returns a Failure if the node was not found") {
    when(extractService.getNodeData(nodeId))
      .thenReturn(Success(sampleNode.copy(contents = Seq())))
    when(draftApiClient.getArticleIdFromExternalId(nodeId)).thenReturn(None)

    val result =
      eCSService.processNode(nodeId, ImportStatus(Seq(), Set("9876")))
    result.isFailure should be(true)
  }

  test("ETL should return a Failure if validation fails") {
    when(draftApiClient.getArticleIdFromExternalId(nodeId))
      .thenReturn(Some(1: Long))
    when(draftApiClient.getArticleIdFromExternalId(nodeId2))
      .thenReturn(Some(2: Long))
    when(draftApiClient.publishArticle(any[Long]))
      .thenReturn(Failure(new HttpRequestException("validation")))
    when(draftApiClient.deleteArticle(1))
      .thenReturn(Success(ContentId(1: Long)))
    when(draftApiClient.getArticleFromId(1))
      .thenReturn(Some(TestData.sampleApiArticle.copy(revision = Some(1))))

    val result = eCSService.processNode(nodeId, ImportStatus.empty)

    result.isFailure should be(true)
    verify(draftApiClient, times(1)).deleteArticle(1)
    verify(draftApiClient, times(0)).deleteConcept(1)
  }

  test("That ETL returns a Failure if failed to persist the converted article") {
    when(draftApiClient.updateArticle(any[Article], any[String], any[Set[String]]))
      .thenReturn(Failure(new OptimisticLockException()))
    when(draftApiClient.getArticleIdFromExternalId(sampleNode.contents.head.nid))
      .thenReturn(Some(1: Long))
    when(draftApiClient.getArticleIdFromExternalId(nodeId)).thenReturn(None)
    when(draftApiClient.newArticle(any[Article], any[String], any[Set[String]]))
      .thenReturn(Failure(new HttpRequestException("store")))
    when(draftApiClient.getConceptIdFromExternalId(any[String]))
      .thenReturn(Some(1: Long))
    when(draftApiClient.publishArticle(any[Long]))
      .thenReturn(Success(ArticleStatus(Set("IMPORTED", "PUBLISHED"))))

    val result =
      eCSService.processNode(nodeId, ImportStatus(Seq(), Set("9876")))
    result.isFailure should be(true)
  }

  test("Articles that fails to import should be deleted from database if it exists") {
    reset(draftApiClient)
    when(draftApiClient.getArticleIdFromExternalId(any[String]))
      .thenReturn(Some(1: Long))
    when(draftApiClient.deleteArticle(1))
      .thenReturn(Success(ContentId(1: Long)))
    when(draftApiClient.getArticleFromId(1))
      .thenReturn(Some(TestData.sampleApiArticle.copy(revision = Some(1))))

    val result = eCSService.processNode(nodeId, ImportStatus.empty)
    result.isFailure should be(true)
    verify(draftApiClient, times(1)).deleteArticle(1)
    verify(draftApiClient, times(0)).deleteConcept(1)
  }

  test("Articles should be force-updated if flag is set") {
    reset(draftApiClient)
    val sampleArticle = TestData.sampleApiArticle.copy(revision = Some(10))
    when(extractConvertStoreContent.processNode(nodeId2, ImportStatus.empty(forceUpdate = true).addVisitedNode(nodeId)))
      .thenReturn(Try((sampleArticle, ImportStatus(Seq(), Set(nodeId, nodeId2), Some(1), false, true))))
    when(draftApiClient.getArticleIdFromExternalId(any[String]))
      .thenReturn(Some(1: Long))
    when(draftApiClient.getContentByExternalId(any[String]))
      .thenReturn(Some(sampleArticle))
    when(draftApiClient.updateArticle(any[Article], any[String], any[Set[String]]))
      .thenReturn(Success(TestData.sampleApiArticle))

    when(draftApiClient.getConceptIdFromExternalId(any[String]))
      .thenReturn(Some(1: Long))
    when(draftApiClient.publishArticle(any[Long]))
      .thenReturn(Success(ArticleStatus(Set("IMPORTED", "PUBLISHED"))))

    val Success((_, status)) =
      eCSService.processNode(nodeId, ImportStatus.empty(forceUpdate = true))
    status should equal(
      ImportStatus(List(s"Successfully imported node $nodeId: 1"), Set(nodeId), Some(sampleArticle.id), false, true))

    verify(draftApiClient, times(1))
      .updateArticle(any[Article], any[String], any[Set[String]])
  }

  test("Articles should not be force-updated if flag is not set") {
    val sampleArticle = TestData.sampleApiArticle.copy(revision = Some(10))
    reset(draftApiClient)
    when(
      extractConvertStoreContent.processNode(nodeId2, ImportStatus.empty(forceUpdate = false).addVisitedNode(nodeId)))
      .thenReturn(Try((sampleArticle, ImportStatus(Seq(), Set(nodeId, nodeId2), Some(1), false, false))))
    when(draftApiClient.getArticleIdFromExternalId(any[String]))
      .thenReturn(Some(1: Long))
    when(draftApiClient.getContentByExternalId(any[String]))
      .thenReturn(Some(sampleArticle))
    when(draftApiClient.getArticleFromId(any[Long]))
      .thenReturn(Some(sampleArticle))
    when(draftApiClient.updateArticle(any[Article], any[String], any[Set[String]]))
      .thenReturn(Success(TestData.sampleApiArticle))

    when(draftApiClient.getConceptIdFromExternalId(any[String]))
      .thenReturn(Some(1: Long))
    when(draftApiClient.publishArticle(any[Long]))
      .thenReturn(Success(ArticleStatus(Set("IMPORTED", "PUBLISHED"))))

    val Success((_, status)) =
      eCSService.processNode(nodeId, ImportStatus.empty(forceUpdate = false))
    status should equal(
      ImportStatus(
        List(s"$nodeId has been updated since import, refusing to import.", s"Successfully imported node $nodeId: 1"),
        Set(nodeId),
        Some(sampleArticle.id),
        false,
        false
      ))
    verify(draftApiClient, times(0))
      .updateArticle(any[Article], any[String], any[Set[String]])
  }

  test("storeArticle should update if id exists, but no body") {
    val id = 1234
    val sampleArticle =
      TestData.sampleArticleWithPublicDomain.copy(id = Some(id))
    reset(draftApiClient)

    when(draftApiClient.getArticleIdFromExternalId(id.toString))
      .thenReturn(Some(10: Long))
    when(draftApiClient.getArticleFromId(10)).thenReturn(None)
    when(draftApiClient.publishArticle(any[Long]))
      .thenReturn(Success(ArticleStatus(Set("IMPORTED", "PUBLISHED"))))
    when(draftApiClient.updateArticle(any[Article], any[String], any[Set[String]]))
      .thenReturn(Success(TestData.sampleApiArticle.copy(id = id)))

    eCSService.storeArticle(sampleArticle, id.toString, ImportStatus.empty)

    verify(draftApiClient, times(1))
      .updateArticle(any[Article], any[String], any[Set[String]])
    verify(draftApiClient, times(0))
      .newArticle(any[Article], any[String], any[Set[String]])
  }

  test("storeArticle should create new if id does not exist at all") {
    val id = 1234
    val sampleArticle =
      TestData.sampleArticleWithPublicDomain.copy(id = Some(id))
    reset(draftApiClient)

    when(draftApiClient.getArticleIdFromExternalId(id.toString))
      .thenReturn(None)
    when(draftApiClient.getArticleFromId(10)).thenReturn(None)
    when(draftApiClient.publishArticle(any[Long]))
      .thenReturn(Success(ArticleStatus(Set("IMPORTED", "PUBLISHED"))))
    when(draftApiClient.newArticle(any[Article], any[String], any[Set[String]]))
      .thenReturn(Success(TestData.sampleApiArticle.copy(id = id)))

    eCSService.storeArticle(sampleArticle, id.toString, ImportStatus.empty)

    verify(draftApiClient, times(0))
      .updateArticle(any[Article], any[String], any[Set[String]])
    verify(draftApiClient, times(1))
      .newArticle(any[Article], any[String], any[Set[String]])
  }

}
