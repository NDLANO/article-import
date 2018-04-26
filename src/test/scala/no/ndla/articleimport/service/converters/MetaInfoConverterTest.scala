/*
 * Part of NDLA article_import.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters

import java.util.Date
import no.ndla.articleimport.model.domain._
import no.ndla.articleimport.{TestData, TestEnvironment, UnitSuite}
import no.ndla.articleimport.ArticleImportProperties.nodeTypeLink
import org.mockito.Mockito._

class MetaInfoConverterTest extends UnitSuite with TestEnvironment {

  val contentTitle = ArticleTitle("", "unknown")
  val author = Author("forfatter", "Henrik")
  val tag = ArticleTag(List("asdf"), "nb")

  val sampleNode = NodeToConvert(List(contentTitle),
                                 Seq(),
                                 Some("by-sa"),
                                 Seq(author),
                                 List(tag),
                                 "fagstoff",
                                 "fagstoff",
                                 new Date(0),
                                 new Date(1),
                                 ArticleType.Standard,
                                 Seq.empty)

  test("toDomainArticle should import ingress images and use as meta images (yes really)") {
    val (imageId, imageNid) = ("1", "1234")
    val contents = Seq(TestData.sampleContent.copy(metaImage = Some(imageNid), language = "nb"))

    when(imageApiClient.importImage(imageNid))
      .thenReturn(Some(TestData.sampleImageMetaInformation.copy(id = imageId)))
    val node = sampleNode.copy(contents = contents)

    MetaInfoConverter.convert(node, ImportStatus.empty)._1.metaImages should be(Seq(ArticleMetaImage(imageId, "nb")))
  }

  test("That license should be by-sa by default if lenkenode") {
    val node = sampleNode.copy(license = None, nodeType = nodeTypeLink)

    MetaInfoConverter.convert(node, ImportStatus.empty)._1.license should be("by-sa")
  }

}
