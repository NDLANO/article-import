/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport

import no.ndla.articleimport.integration._
import no.ndla.articleimport.model.domain._
import no.ndla.articleimport.model.api
import org.joda.time.DateTime

object TestData {
  private val publicDomainCopyright= Copyright("publicdomain", None, Seq.empty, Seq.empty, Seq.empty, None, None, None)
  private val byNcSaCopyright = Copyright("by-nc-sa", Some("Gotham City"), Seq.empty, Seq.empty, Seq.empty, None, None, None)
  private val today = new DateTime().toDate

  val articleId = 1

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
    None,
    DateTime.now().minusDays(4).toDate,
    DateTime.now().minusDays(2).toDate,
    "ndalId54321",
    ArticleType.Standard.toString)

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
    Some("11"),
    today,
    today,
    "ndalId54321",
    ArticleType.Standard.toString
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
    None,
    today,
    today,
    "ndalId54321",
    ArticleType.Standard.toString
  )

  val newArticleV2 = api.NewArticle(
    "test",
    "<article><div>test</div></article>",
    Seq(),
    None,
    None,
    None,
    None,
    api.Copyright(Some(api.License("publicdomain", None, None)), Some(""), Seq.empty, Seq.empty, Seq.empty, None, None, None),
    Seq.empty,
    "standard",
    "en"
  )

  val newArticleV2Body = api.NewArticle(
    "title",
    "content",
    Seq("tag"),
    Some("introductino"),
    Some("metadescription"),
    Some("22"),
    None,
    api.Copyright(Some(api.License("by-sa", None, None)), Some("fromSomeWhere"), Seq(api.Author("string", "du")), Seq.empty, Seq.empty, None, None, None),
    Seq.empty,
    "standard",
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
    None
  )

  val sampleArticleWithByNcSa = sampleArticleWithPublicDomain.copy(copyright=byNcSaCopyright)

  val (nodeId, nodeId2) = ("1234", "4321")
  val sampleTitle = ArticleTitle("title", "en")
  val sampleContent = LanguageContent(nodeId, nodeId, "sample content", "metadescription", "en", None, "fagstoff", Some("title"), Seq.empty)
  val sampleTranslationContent = sampleContent.copy(tnid=nodeId2)

  val sampleImageMetaInformation = ImageMetaInformation(
    "1",
    List(ImageTitle("Sample title", Some("nb"))),
    List(ImageAltText("alt text", Some("nb"))),
    "://image-url.com/image/img.jpg",
    1024,
    "application/jpeg",
    ImageCopyright(ImageLicense("by", "Creative Commons", None), "pix", Seq.empty),
    ImageTag(Seq("sample tag"), Some("nb")))

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
    api.ConceptTitle("Tittel for begrep", "nb"),
    api.ConceptContent("Innhold for begrep", "nb"),
    Some(api.Copyright(Some(api.License("publicdomain", None, None)), Some(""), Seq.empty, Seq.empty, Seq.empty, None, None, None)),
    DateTime.now().minusDays(4).toDate,
    DateTime.now().minusDays(2).toDate,
    Set("nb")
  )

  val sampleApiArticle = api.Article(
    articleId,
    None,
    1,
    api.ArticleTitle("tittel", "nb"),
    api.ArticleContent("innhold", "nb"),
    api.Copyright(Some(api.License("by-sa", None, None)), Some("fromSomeWhere"), Seq(api.Author("string", "du")), Seq.empty, Seq.empty, None, None, None),
    api.ArticleTag(Seq.empty, "nb"),
    Seq.empty,
    None,
    None,
    None,
    api.ArticleMetaDescription("metabeskrivelse", "nb"),
    today,
    today,
    "me",
    "standard",
    Seq("nb")
  )

}

