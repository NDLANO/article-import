/*
 * Part of NDLA article_import.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters

import no.ndla.articleimport.ArticleImportProperties.nodeTypeLink
import no.ndla.articleimport.TestData._
import no.ndla.articleimport.integration.{ImageAltText, TaxonomyApiClient}
import no.ndla.articleimport.model.api.ImportException
import no.ndla.articleimport.model.domain._
import no.ndla.articleimport.{TestData, TestEnvironment, UnitSuite}
import no.ndla.mapping.License._
import org.mockito.Mockito._

import scala.util.{Failure, Success}

class MetaInfoConverterTest extends UnitSuite with TestEnvironment {

  test("toDomainArticle should import ingress images and use as meta images (yes really)") {
    val (imageId, imageNid) = ("1", "1234")
    val contents = Seq(TestData.sampleContent.copy(metaImage = Some(imageNid), language = "nb"))
    val image = Some(TestData.sampleImageMetaInformation.copy(id = imageId))

    when(imageApiClient.importImage(imageNid)).thenReturn(image)
    when(imageApiClient.getMetaByExternId(imageNid, "nb")).thenReturn(image)
    val node = sampleNodeToConvert.copy(contents = contents)

    MetaInfoConverter.convert(node, ImportStatus.empty).get._1.metaImages should be(
      Seq(ArticleMetaImage(imageId, "alt text", "nb")))
  }

  test("That license should be CC-BY-SA-4.0 by default if lenkenode") {
    val node = sampleNodeToConvert.copy(license = None, nodeType = nodeTypeLink)

    MetaInfoConverter.convert(node, ImportStatus.empty).get._1.license should be(CC_BY_SA.toString)
  }

  test("That import should fail if inserted licenses cannot be merged with original license") {
    val insertedAuthors = List(author)
    val status = ImportStatus.empty
      .addInsertedAuthors(insertedAuthors)
      .addInsertedLicense(Some(CC_BY_SA.toString))
      .addInsertedLicense(Some(Copyrighted.toString))

    val result = MetaInfoConverter.convert(sampleNodeToConvert, status)

    result.isFailure should be(true)
    val Failure(ex: ImportException) = result

    ex.message should be("Could not combine license CC-BY-SA-4.0 with inserted licenses: CC-BY-SA-4.0,COPYRIGHTED.")
  }

  test("That import should combine licenses") {
    val status1 =
      ImportStatus.empty.addInsertedLicense(Some(CC_BY.toString)).addInsertedLicense(Some(CC_BY_NC_SA.toString))
    val Success((converted1, _)) = MetaInfoConverter.convert(sampleNodeToConvert, status1)
    converted1.license should be(CC_BY_NC_SA.toString)

    val status2 =
      ImportStatus.empty.addInsertedLicense(Some(CC_BY_NC_SA.toString)).addInsertedLicense(Some(CC_BY_NC_SA.toString))
    val Success((converted2, _)) = MetaInfoConverter.convert(sampleNodeToConvert, status2)
    converted2.license should be(CC_BY_NC_SA.toString)

    val status3 = ImportStatus.empty
    val Success((converted3, _)) = MetaInfoConverter.convert(sampleNodeToConvert, status3)
    converted3.license should be(CC_BY_SA.toString)

    val status4 = ImportStatus.empty
    val Success((converted4, _)) =
      MetaInfoConverter.convert(sampleNodeToConvert.copy(license = Some(Copyrighted.toString)), status4)
    converted4.license should be(Copyrighted.toString)

    val status5 = ImportStatus.empty
      .addInsertedLicense(Some(CC_BY.toString))
      .addInsertedLicense(Some(CC_BY_ND.toString))
      .addInsertedLicense(Some(CC_BY_NC.toString))
    val Success((converted5, _)) =
      MetaInfoConverter.convert(sampleNodeToConvert.copy(license = Some(CC_BY.toString)), status5)
    converted5.license should be(CC_BY_NC_ND.toString)

    val status6 = ImportStatus.empty
      .addInsertedLicense(Some(CC_BY_SA.toString))
      .addInsertedLicense(Some(CC_BY_ND.toString))
      .addInsertedLicense(Some(CC_BY_NC.toString))
    val Failure(ex6: ImportException) = MetaInfoConverter.convert(sampleNodeToConvert, status6)
    ex6.message should be(
      s"Could not combine license CC-BY-SA-4.0 with inserted licenses: CC-BY-SA-4.0,CC-BY-ND-4.0,CC-BY-NC-4.0.")
  }

  test("That import should combine license and authors from inserted nodes") {
    val insertedAuthors = List(Author("forfatter", "Jonas"), Author("fotograf", "Christian"))
    val status = ImportStatus.empty
      .addInsertedAuthors(insertedAuthors)
      .addInsertedLicense(Some(CC_BY_SA.toString))
      .addInsertedLicense(Some(CC_BY_NC_SA.toString))

    val Success((converted, _)) = MetaInfoConverter.convert(sampleNodeToConvert, status)

    converted.license should be(CC_BY_NC_SA.toString)
    converted.authors.map(_.name).sorted should be(Seq("Christian", "Henrik", "Jonas"))
  }

  test("That license combination does not break other licenses") {
    val status1 = ImportStatus.empty.addInsertedLicense(Some("pd"))
    val Success((converted1, _)) = MetaInfoConverter.convert(sampleNodeToConvert.copy(license = Some("pd")), status1)
    converted1.license should be("pd")
  }

  test("That metaImages are only imported once") {
    reset(imageApiClient)
    val (imageId, imageNid) = ("1", "1234")
    val nbContents = TestData.sampleContent.copy(metaImage = Some(imageNid), language = "nb")
    val enContents = TestData.sampleContent.copy(metaImage = Some(imageNid), language = "en")
    val image = TestData.sampleImageMetaInformation.copy(id = imageId)
    val nbImage = image.copy(alttext = Some(ImageAltText("nbAlt", "nb")))
    val enImage = image.copy(alttext = Some(ImageAltText("enAlt", "en")))
    val node = sampleNodeToConvert.copy(contents = Seq(nbContents, enContents))

    when(imageApiClient.importImage(imageNid)).thenReturn(Some(image))
    when(imageApiClient.getMetaByExternId(imageNid, "nb")).thenReturn(Some(nbImage))
    when(imageApiClient.getMetaByExternId(imageNid, "en")).thenReturn(Some(enImage))

    val (converted, _) = MetaInfoConverter.convert(node, ImportStatus.empty).get

    converted.metaImages should be(
      Seq(ArticleMetaImage(nbImage.id, nbImage.alttext.get.alttext, "nb"),
          ArticleMetaImage(enImage.id, enImage.alttext.get.alttext, "en")))

    verify(imageApiClient, times(1)).importImage(imageNid)
  }

  test("That types are derived correctly from taxonomy") {
    when(taxonomyApiClient.getResource("1111"))
      .thenReturn(Success(Some(TaxonomyApiClient.Resource("", "", Some(""), ""))))
    when(taxonomyApiClient.getTopic("1111")).thenReturn(Success(None))
    when(taxonomyApiClient.getResource("2222")).thenReturn(Success(None))
    when(taxonomyApiClient.getTopic("2222")).thenReturn(Success(None))

    val t1 = MetaInfoConverter.articleTypeFromTaxonomy(Seq("1111", "2222"), ArticleType.Standard, ImportStatus.empty)
    t1._1 should be(ArticleType.Standard)

    val t2 =
      MetaInfoConverter.articleTypeFromTaxonomy(Seq("1111", "2222"), ArticleType.TopicArticle, ImportStatus.empty)
    t2._1 should be(ArticleType.Standard)

    when(taxonomyApiClient.getResource("1111")).thenReturn(Success(None))
    when(taxonomyApiClient.getTopic("1111")).thenReturn(Success(Some(TaxonomyApiClient.Topic("", "", Some(""), ""))))
    when(taxonomyApiClient.getResource("2222")).thenReturn(Success(None))
    when(taxonomyApiClient.getTopic("2222")).thenReturn(Success(None))

    val t3 = MetaInfoConverter.articleTypeFromTaxonomy(Seq("1111", "2222"), ArticleType.Standard, ImportStatus.empty)
    t3._1 should be(ArticleType.TopicArticle)

    val t4 =
      MetaInfoConverter.articleTypeFromTaxonomy(Seq("1111", "2222"), ArticleType.TopicArticle, ImportStatus.empty)
    t4._1 should be(ArticleType.TopicArticle)
  }

  test("Type should be fetched from migration-api if taxonomy api is not consistent") {
    when(taxonomyApiClient.getResource("1111"))
      .thenReturn(Success(Some(TaxonomyApiClient.Resource("", "", Some(""), ""))))
    when(taxonomyApiClient.getTopic("2222")).thenReturn(Success(Some(TaxonomyApiClient.Topic("", "", Some(""), ""))))
    when(taxonomyApiClient.getResource("2222")).thenReturn(Success(None))
    when(taxonomyApiClient.getTopic("1111")).thenReturn(Success(None))

    val t1 = MetaInfoConverter.articleTypeFromTaxonomy(Seq("1111", "2222"), ArticleType.Standard, ImportStatus.empty)
    t1._1 should be(ArticleType.Standard)
    t1._2.errors.head.message should be(
      s"Article with nids '1111, 2222' have multiple article types in taxonomy, using type from migration: '${ArticleType.Standard}'.")

    val t2 =
      MetaInfoConverter.articleTypeFromTaxonomy(Seq("1111", "2222"), ArticleType.TopicArticle, ImportStatus.empty)
    t2._1 should be(ArticleType.TopicArticle)
    t2._2.errors.head.message should be(
      s"Article with nids '1111, 2222' have multiple article types in taxonomy, using type from migration: '${ArticleType.TopicArticle}'.")
  }

}
