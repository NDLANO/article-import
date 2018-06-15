/*
 * Part of NDLA article_import.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters

import no.ndla.articleimport.model.domain._
import no.ndla.articleimport.{TestData, TestEnvironment, UnitSuite}
import no.ndla.articleimport.ArticleImportProperties.nodeTypeLink
import no.ndla.articleimport.model.api.ImportException
import no.ndla.articleimport.TestData._
import org.mockito.Mockito._
import scala.util.{Failure, Success}

class MetaInfoConverterTest extends UnitSuite with TestEnvironment {

  test("toDomainArticle should import ingress images and use as meta images (yes really)") {
    val (imageId, imageNid) = ("1", "1234")
    val contents = Seq(TestData.sampleContent.copy(metaImage = Some(imageNid), language = "nb"))

    when(imageApiClient.importImage(imageNid))
      .thenReturn(Some(TestData.sampleImageMetaInformation.copy(id = imageId)))
    val node = sampleNodeToConvert.copy(contents = contents)

    MetaInfoConverter.convert(node, ImportStatus.empty).get._1.metaImages should be(
      Seq(ArticleMetaImage(imageId, "nb")))
  }

  test("That license should be by-sa by default if lenkenode") {
    val node = sampleNodeToConvert.copy(license = None, nodeType = nodeTypeLink)

    MetaInfoConverter.convert(node, ImportStatus.empty).get._1.license should be("by-sa")
  }

  test("That import should fail if inserted licenses cannot be merged with original license") {
    val insertedAuthors = List(author)
    val status = ImportStatus.empty
      .addInsertedAuthors(insertedAuthors)
      .addInsertedLicense(Some("by-sa"))
      .addInsertedLicense(Some("copyrighted"))

    val result = MetaInfoConverter.convert(sampleNodeToConvert, status)

    result.isFailure should be(true)
    val Failure(ex: ImportException) = result

    ex.message should be("Could not combine license by-sa with inserted licenses: by-sa,copyrighted.")
  }

  test("That import should combine licenses") {
    val status1 = ImportStatus.empty.addInsertedLicense(Some("by-sa")).addInsertedLicense(Some("by-nc-sa"))
    val Success((converted1, _)) = MetaInfoConverter.convert(sampleNodeToConvert, status1)
    converted1.license should be("by-nc-sa")

    val status2 = ImportStatus.empty.addInsertedLicense(Some("by-nc-sa")).addInsertedLicense(Some("by-nc-sa"))
    val Success((converted2, _)) = MetaInfoConverter.convert(sampleNodeToConvert, status2)
    converted2.license should be("by-nc-sa")

    val status3 = ImportStatus.empty
    val Success((converted3, _)) = MetaInfoConverter.convert(sampleNodeToConvert, status3)
    converted3.license should be("by-sa")

    val status4 = ImportStatus.empty
    val Success((converted4, _)) =
      MetaInfoConverter.convert(sampleNodeToConvert.copy(license = Some("copyrighted")), status4)
    converted4.license should be("copyrighted")

    val status5 = ImportStatus.empty
      .addInsertedLicense(Some("by"))
      .addInsertedLicense(Some("by-nd"))
      .addInsertedLicense(Some("by-nc"))
    val Success((converted5, _)) = MetaInfoConverter.convert(sampleNodeToConvert.copy(license = Some("by")), status5)
    converted5.license should be("by-nc-nd")

    val status6 = ImportStatus.empty
      .addInsertedLicense(Some("by-sa"))
      .addInsertedLicense(Some("by-nd"))
      .addInsertedLicense(Some("by-nc"))
    val Failure(ex6: ImportException) = MetaInfoConverter.convert(sampleNodeToConvert, status6)
    ex6.message should be(s"Could not combine license by-sa with inserted licenses: by-sa,by-nd,by-nc.")
  }

  test("That import should combine license and authors from inserted nodes") {
    val insertedAuthors = List(Author("forfatter", "Jonas"), Author("fotograf", "Christian"))
    val status = ImportStatus.empty
      .addInsertedAuthors(insertedAuthors)
      .addInsertedLicense(Some("by-sa"))
      .addInsertedLicense(Some("by-nc-sa"))

    val Success((converted, _)) = MetaInfoConverter.convert(sampleNodeToConvert, status)

    converted.license should be("by-nc-sa")
    converted.authors.map(_.name).sorted should be(Seq("Christian", "Henrik", "Jonas"))
  }

  test("That license combination does not break other licenses") {
    val status1 = ImportStatus.empty.addInsertedLicense(Some("pd"))
    val Success((converted1, _)) = MetaInfoConverter.convert(sampleNodeToConvert.copy(license = Some("pd")), status1)
    converted1.license should be("pd")
  }

}
