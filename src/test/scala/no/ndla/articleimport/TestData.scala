/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport

import java.util.Date

import no.ndla.articleimport.integration._
import no.ndla.articleimport.model.domain._
import no.ndla.articleimport.model.api
import no.ndla.articleimport.model.api.{ArticleStatus, NewArticleMetaImage}
import no.ndla.articleimport.service.converters.contentbrowser.ContentBrowser
import org.joda.time.DateTime

object TestData {
  private val publicDomainCopyright = Copyright("publicdomain", None, Seq.empty, Seq.empty, Seq.empty, None, None, None)
  private val byNcSaCopyright =
    Copyright("by-nc-sa", Some("Gotham City"), Seq.empty, Seq.empty, Seq.empty, None, None, None)
  private val today = new DateTime().toDate

  val articleId = 1

  val sampleImportedPublishedStatus = ArticleStatus("PUBLISHED", Set("IMPORTED"))

  val sampleArticleWithPublicDomain = Article(
    Option(1),
    Option(1),
    Seq(ArticleTitle("test", "en")),
    Seq(ArticleContent("<section><div>test</div></section>", "en")),
    publicDomainCopyright,
    Seq(),
    Seq(),
    Seq(VisualElement("image", "en")),
    Seq(ArticleIntroduction("This is an introduction", "en")),
    Seq(),
    Seq.empty,
    DateTime.now().minusDays(4).toDate,
    DateTime.now().minusDays(2).toDate,
    "ndalId54321",
    ArticleType.Standard.toString,
    Seq.empty
  )

  val sampleDomainArticle = Article(
    Option(articleId),
    Option(2),
    Seq(ArticleTitle("title", "nb")),
    Seq(ArticleContent("content", "nb")),
    Copyright("by", Some(""), Seq.empty, Seq.empty, Seq.empty, None, None, None),
    Seq(),
    Seq(),
    Seq(),
    Seq(),
    Seq(ArticleMetaDescription("meta description", "nb")),
    Seq(ArticleMetaImage("11", "alt", "nb")),
    today,
    today,
    "ndalId54321",
    ArticleType.Standard.toString,
    Seq.empty
  )

  val sampleDomainArticle2 = Article(
    None,
    None,
    Seq(ArticleTitle("test", "en")),
    Seq(ArticleContent("<article><div>test</div></article>", "en")),
    Copyright("publicdomain", Some(""), Seq.empty, Seq.empty, Seq.empty, None, None, None),
    Seq(),
    Seq(),
    Seq(),
    Seq(),
    Seq(),
    Seq.empty,
    today,
    today,
    "ndalId54321",
    ArticleType.Standard.toString,
    Seq.empty
  )

  val newArticleV2 = api.NewArticle(
    "test",
    "<article><div>test</div></article>",
    Seq(),
    None,
    None,
    None,
    None,
    api.Copyright(Some(api.License("publicdomain", None, None)),
                  Some(""),
                  Seq.empty,
                  Seq.empty,
                  Seq.empty,
                  None,
                  None,
                  None),
    Seq.empty,
    "standard",
    Seq.empty,
    "en"
  )

  val newArticleV2Body = api.NewArticle(
    "title",
    "content",
    Seq("tag"),
    Some("introductino"),
    Some("metadescription"),
    Some(
      NewArticleMetaImage("22", "alt")
    ),
    None,
    api.Copyright(Some(api.License("by-sa", None, None)),
                  Some("fromSomeWhere"),
                  Seq(api.Author("string", "du")),
                  Seq.empty,
                  Seq.empty,
                  None,
                  None,
                  None),
    Seq.empty,
    "standard",
    Seq.empty,
    "nb"
  )

  val updatedArticleV2 = api.UpdateArticle(
    1,
    "nb",
    Some("updated title"),
    None,
    Seq.empty,
    None,
    None,
    None,
    None,
    None,
    Seq.empty,
    Seq.empty,
    None
  )

  val sampleArticleWithByNcSa =
    sampleArticleWithPublicDomain.copy(copyright = byNcSaCopyright)

  val (nodeId, nodeId2) = ("1234", "4321")
  val sampleTitle = ArticleTitle("title", "en")

  val sampleContent = LanguageContent(nodeId,
                                      nodeId,
                                      "sample content",
                                      "metadescription",
                                      "en",
                                      None,
                                      "fagstoff",
                                      Some("title"),
                                      Seq.empty,
                                      None)
  val sampleTranslationContent = sampleContent.copy(tnid = nodeId2)

  val sampleImageMetaInformation = ImageMetaInformation(
    "1",
    Some(ImageTitle("Sample title", "nb")),
    Some(ImageAltText("alt text", "nb")),
    "://image-url.com/image/img.jpg",
    1024,
    "application/jpeg",
    ImageCopyright(ImageLicense("by", "Creative Commons", None), "pix", Seq.empty),
    ImageTag(Seq("sample tag"), "nb")
  )

  val sampleConcept = Concept(
    Some(1),
    Seq(ConceptTitle("Tittel for begrep", "nb")),
    Seq(ConceptContent("Innhold for begrep", "nb")),
    Some(Copyright("publicdomain", Some(""), Seq.empty, Seq.empty, Seq.empty, None, None, None)),
    DateTime.now().minusDays(4).toDate,
    DateTime.now().minusDays(2).toDate
  )

  val sampleApiConcept = api.Concept(
    1,
    None,
    Some(api.ConceptTitle("Tittel for begrep", "nb")),
    Some(api.ConceptContent("Innhold for begrep", "nb")),
    Some(
      api.Copyright(Some(api.License("publicdomain", None, None)),
                    Some(""),
                    Seq.empty,
                    Seq.empty,
                    Seq.empty,
                    None,
                    None,
                    None)),
    DateTime.now().minusDays(4).toDate,
    DateTime.now().minusDays(2).toDate,
    Set("nb")
  )

  val sampleApiArticle = api.Article(
    articleId,
    None,
    Some(1),
    api.ArticleTitle("tittel", "nb"),
    Some(api.ArticleContent("innhold", "nb")),
    Some(
      api.Copyright(Some(api.License("by-sa", None, None)),
                    Some("fromSomeWhere"),
                    Seq(api.Author("string", "du")),
                    Seq.empty,
                    Seq.empty,
                    None,
                    None,
                    None)),
    Some(api.ArticleTag(Seq.empty, "nb")),
    Seq.empty,
    None,
    None,
    None,
    Some(api.ArticleMetaDescription("metabeskrivelse", "nb")),
    today,
    today,
    "me",
    "standard",
    Seq("nb"),
    ArticleStatus("DRAFT", Set.empty)
  )

  val contentTitle = ArticleTitle("", "unknown")
  val author = Author("forfatter", "Henrik")
  val tag = ArticleTag(List("asdf"), "nb")

  val sampleNodeToConvert = NodeToConvert(List(contentTitle),
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

  val sampleTaxonomyResource = TaxonomyApiClient.Resource("1234",
                                                          "Sample resource",
                                                          None,
                                                          "/subject:12/topic:1:184105/topic:1:184106/resource:1:77480")
  val sampleTaxonomyTopic = TaxonomyApiClient.Topic("12345", "Sample topic", None, "/subject:7/topic:1:183193")

  def contentBrowserWithFields(dOMPath: List[String], fields: (String, String)*): ContentBrowser = {
    new ContentBrowser {
      val FieldMap = fields.toMap
      override def getOpt(key: String): Option[String] = FieldMap.get(key)
      override def get(key: String): String = getOpt(key).getOrElse("")
      override val language: String = "nb"
      override val DOMPath: List[String] = dOMPath
    }
  }

}
