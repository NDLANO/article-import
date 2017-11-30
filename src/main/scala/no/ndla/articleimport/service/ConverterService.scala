/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.articleimport.service

import com.typesafe.scalalogging.LazyLogging
import no.ndla.articleimport.ArticleImportProperties._
import no.ndla.articleimport.auth.User
import no.ndla.articleimport.integration.ConverterModule.{jsoupDocumentToString, stringToJsoupDocument}
import no.ndla.articleimport.integration.ImageApiClient
import no.ndla.articleimport.model.api
import no.ndla.articleimport.model.domain.Language._
import no.ndla.articleimport.model.domain._
import no.ndla.mapping.License.getLicense
import no.ndla.validation.{Attributes, EmbedTagRules, HtmlRules, ResourceType}

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

trait ConverterService {
  this: ConverterModules with ExtractConvertStoreContent with ImageApiClient with Clock with User =>
  val converterService: ConverterService

  class ConverterService extends LazyLogging {

    def toDomainArticle(nodeToConvert: NodeToConvert, importStatus: ImportStatus): Try[(Content, ImportStatus)] = {
      val nodeIdsToImport = nodeToConvert.contents.map(_.nid).toSet

      convert(nodeToConvert, maxConvertionRounds, importStatus.addVisitedNodes(nodeIdsToImport))
          .flatMap { case (content, status) => postProcess(content, status) } match {
        case Failure(f) => Failure(f)
        case Success((convertedContent, converterStatus)) if convertedContent.nodeType == nodeTypeBegrep =>
          Success((toDomainConcept(convertedContent), converterStatus))
        case Success((convertedContent, converterStatus)) => Success((toDomainArticle(convertedContent), converterStatus))
      }
    }

    @tailrec private def convert(nodeToConvert: NodeToConvert, maxRoundsLeft: Int, importStatus: ImportStatus): Try[(NodeToConvert, ImportStatus)] = {
      if (maxRoundsLeft == 0) {
        val message = "Maximum number of converter rounds reached; Some content might not be converted"
        logger.warn(message)
        return Success((nodeToConvert, importStatus.copy(messages=importStatus.messages :+ message)))
      }

      val (updatedContent, updatedStatus) = executeConverterModules(nodeToConvert, importStatus) match {
        case Failure(e) => return Failure(e)
        case Success(s) => s
      }

      // If this converting round did not yield any changes to the content, this node is finished (case true)
      // If changes were made during this convertion, we run the converters again (case false)
      updatedContent == nodeToConvert match {
        case true => Success((updatedContent, updatedStatus))
        case false => convert(updatedContent, maxRoundsLeft - 1, updatedStatus)
      }
    }

    private def postProcess(nodeToConvert: NodeToConvert, importStatus: ImportStatus): Try[(NodeToConvert, ImportStatus)] =
      executePostprocessorModules(nodeToConvert, importStatus)


    private[service] def toDomainArticle(nodeToConvert: NodeToConvert): Article = {
      val requiredLibraries = nodeToConvert.contents.flatMap(_.requiredLibraries).distinct
      val ingresses = nodeToConvert.contents.flatMap(content => content.asArticleIntroduction)
      val visualElements = nodeToConvert.contents.flatMap(_.asVisualElement)

      val languagesInNode: Set[String] = (nodeToConvert.titles.map(_.language) ++
        nodeToConvert.contents.map(_.language) ++
        ingresses.map(_.language)).toSet

      Article(None,
        None,
        nodeToConvert.titles,
        nodeToConvert.contents.map(_.asContent),
        toDomainCopyright(nodeToConvert.license, nodeToConvert.authors),
        nodeToConvert.tags.filter(tag => languagesInNode.contains(tag.language)),
        requiredLibraries,
        visualElements,
        ingresses,
        nodeToConvert.contents.map(_.asArticleMetaDescription),
        None,
        nodeToConvert.created,
        nodeToConvert.updated,
        authUser.userOrClientid(),
        nodeToConvert.articleType.toString
      )
    }

    private def toDomainConcept(convertedNode: NodeToConvert): Concept = {
      val license = Option(convertedNode.license).filter(_.nonEmpty)

      Concept(
        None,
        convertedNode.titles.map(title => ConceptTitle(title.title, title.language)),
        convertedNode.contents.map(content => ConceptContent(content.content, content.language)),
        license.map(l => toDomainCopyright(l, convertedNode.authors)),
        convertedNode.created,
        convertedNode.updated
      )
    }

    private def toDomainAuthor(author: Author): Author = {
      val creatorMap = (oldCreatorTypes zip creatorTypes).toMap.withDefaultValue(None)
      val processorMap = (oldProcessorTypes zip processorTypes).toMap.withDefaultValue(None)
      val rightsholderMap = (oldRightsholderTypes zip rightsholderTypes).toMap.withDefaultValue(None)

      (creatorMap(author.`type`.toLowerCase), processorMap(author.`type`.toLowerCase), rightsholderMap(author.`type`.toLowerCase)) match {
        case (t: String, None, None) => Author(t.capitalize, author.name)
        case (None, t: String, None) => Author(t.capitalize, author.name)
        case (None, None, t: String) => Author(t.capitalize, author.name)
        case (_, _, _) => Author(author.`type`, author.name)
      }
    }

    private def toDomainCopyright(license: String, authors: Seq[Author]): Copyright = {
      val origin = authors.find(author => author.`type`.toLowerCase == "opphavsmann").map(_.name)
      val creators = authors.filter(a => oldCreatorTypes.contains(a.`type`.toLowerCase)).map(toDomainAuthor)
      // Filters out processor authors with old type `redaksjonelt` during import process since `redaksjonelt` exists both in processors and creators.
      val processors = authors.filter(a => oldProcessorTypes.contains(a.`type`.toLowerCase)).filterNot(a => a.`type`.toLowerCase == "redaksjonelt").map(toDomainAuthor)
      val rightsholders = authors.filter(a => oldRightsholderTypes.contains(a.`type`.toLowerCase)).map(toDomainAuthor)

      Copyright(license, origin, creators, processors, rightsholders, None, None, None)
    }

    def toDomainTitle(articleTitle: api.ArticleTitle): ArticleTitle = {
      ArticleTitle(articleTitle.title, articleTitle.language)
    }

    def toDomainContent(articleContent: api.ArticleContent): ArticleContent = {
      ArticleContent(removeUnknownEmbedTagAttributes(articleContent.content), articleContent.language)
    }

    def toDomainTag(tag: api.ArticleTag): ArticleTag = {
      ArticleTag(tag.tags, tag.language)
    }

    def toDomainTagV2(tag: Seq[String], language: String): Seq[ArticleTag] = {
      if (tag.isEmpty) {
        Seq.empty[ArticleTag]
      } else {
        Seq(ArticleTag(tag, language))
      }
    }

    def toDomainVisualElement(visual: api.VisualElement): VisualElement = {
      VisualElement(removeUnknownEmbedTagAttributes(visual.visualElement), visual.language)
    }

    def toDomainVisualElementV2(visual: Option[String], language: String): Seq[VisualElement] = {
      if (visual.isEmpty) {
        Seq.empty[VisualElement]
      } else {
        Seq(VisualElement(removeUnknownEmbedTagAttributes(visual.getOrElse("")), language))
      }
    }

    def toDomainIntroduction(intro: api.ArticleIntroduction): ArticleIntroduction = {
      ArticleIntroduction(intro.introduction, intro.language)
    }

    def toDomainIntroductionV2(intro: Option[String], language: String): Seq[ArticleIntroduction] = {
      if (intro.isEmpty) {
        Seq.empty[ArticleIntroduction]
      } else {
        Seq(ArticleIntroduction(intro.getOrElse(""), language))
      }
    }

    def toDomainMetaDescription(meta: api.ArticleMetaDescription): ArticleMetaDescription = {
      ArticleMetaDescription(meta.metaDescription, meta.language)
    }

    def toDomainMetaDescriptionV2(meta: Option[String], language: String): Seq[ArticleMetaDescription]= {
      if (meta.isEmpty) {
        Seq.empty[ArticleMetaDescription]
      } else {
        Seq(ArticleMetaDescription(meta.getOrElse(""), language))
      }
    }

    def toDomainAuthor(author: api.Author): Author = {
      Author(author.`type`, author.name)
    }

    def toDomainRequiredLibraries(requiredLibs: api.RequiredLibrary): RequiredLibrary = {
      RequiredLibrary(requiredLibs.mediaType, requiredLibs.name, requiredLibs.url)
    }

    private def removeUnknownEmbedTagAttributes(html: String): String = {
      val document = stringToJsoupDocument(html)
      document.select("embed").asScala.map(el => {
        ResourceType.valueOf(el.attr(Attributes.DataResource.toString))
          .map(EmbedTagRules.attributesForResourceType)
          .map(knownAttributes => HtmlRules.removeIllegalAttributes(el, knownAttributes.all.map(_.toString)))
      })

      jsoupDocumentToString(document)
    }

    def toApiMetaImage(metaImageId: String): String = {
      s"${externalApiUrls("raw-image")}/$metaImageId"
    }
    def toApiArticleTitle(title: ArticleTitle): api.ArticleTitle = {
      api.ArticleTitle(title.title, title.language)
    }

    def toApiArticleContent(content: ArticleContent): api.ArticleContent = {
      api.ArticleContent(
        content.content,
        content.language
      )
    }

    def toApiCopyright(copyright: Copyright): api.Copyright = {
      api.Copyright(
        Some(toApiLicense(copyright.license)),
        copyright.origin,
        copyright.creators.map(toApiAuthor),
        copyright.processors.map(toApiAuthor),
        copyright.rightsholders.map(toApiAuthor),
        copyright.agreementId,
        copyright.validFrom,
        copyright.validTo
      )
    }

    def toApiLicense(shortLicense: String): api.License = {
      getLicense(shortLicense) match {
        case Some(l) => api.License(l.license, Option(l.description), l.url)
        case None => api.License("unknown", None, None)
      }
    }

    def toApiAuthor(author: Author): api.Author = {
      api.Author(author.`type`, author.name)
    }

    def toApiArticleTag(tag: ArticleTag): api.ArticleTag = {
      api.ArticleTag(tag.tags, tag.language)
    }

    def toApiRequiredLibrary(required: RequiredLibrary): api.RequiredLibrary = {
      api.RequiredLibrary(required.mediaType, required.name, required.url)
    }

    def toApiVisualElement(visual: VisualElement): api.VisualElement = {
      api.VisualElement(visual.resource, visual.language)
    }

    def toApiArticleIntroduction(intro: ArticleIntroduction): api.ArticleIntroduction = {
      api.ArticleIntroduction(intro.introduction, intro.language)
    }

    def toApiArticleMetaDescription(metaDescription: ArticleMetaDescription): api.ArticleMetaDescription= {
      api.ArticleMetaDescription(metaDescription.content, metaDescription.language)
    }

    def createLinkToOldNdla(nodeId: String): String = s"//red.ndla.no/node/$nodeId"

    def toApiConcept(concept: Concept, language: String): api.Concept = {
      val title = findByLanguageOrBestEffort(concept.title, language).map(toApiConceptTitle).getOrElse(api.ConceptTitle("", Language.DefaultLanguage))
      val content = findByLanguageOrBestEffort(concept.content, language).map(toApiConceptContent).getOrElse(api.ConceptContent("", Language.DefaultLanguage))

      api.Concept(
        concept.id.get,
        title,
        content,
        concept.copyright.map(toApiCopyright),
        concept.created,
        concept.updated,
        concept.supportedLanguages
      )
    }

    def toNewApiConcept(concept: Concept, language: String): api.NewConcept = {
      val title = findByLanguageOrBestEffort(concept.title, language).map(_.title).getOrElse("")
      val content = findByLanguageOrBestEffort(concept.content, language).map(_.content).getOrElse("")

      api.NewConcept(
        language,
        title,
        content,
        concept.copyright.map(toApiCopyright)
      )
    }

    def toUpdateApiConcept(concept: Concept, language: String): api.UpdateConcept = {
      val title = findByLanguageOrBestEffort(concept.title, language).map(_.title)
      val content = findByLanguageOrBestEffort(concept.content, language).map(_.content)

      api.UpdateConcept(
        language,
        title,
        content,
        concept.copyright.map(toApiCopyright)
      )
    }

    def toApiConceptTitle(title: ConceptTitle): api.ConceptTitle = api.ConceptTitle(title.title, title.language)

    def toApiConceptContent(title: ConceptContent): api.ConceptContent= api.ConceptContent(title.content, title.language)

    def toApiNewArticle(article: Article, lang: String): api.NewArticle = {
      api.NewArticle(
        findByLanguageOrBestEffort(article.title, lang).map(_.value).getOrElse(""),
        findByLanguageOrBestEffort(article.content, lang).map(_.value).getOrElse(""),
        findByLanguageOrBestEffort(article.tags, lang).map(_.value).getOrElse(Seq.empty),
        findByLanguageOrBestEffort(article.introduction, lang).map(_.value),
        findByLanguageOrBestEffort(article.metaDescription, lang).map(_.value),
        article.metaImageId,
        findByLanguageOrBestEffort(article.visualElement, lang).map(_.value),
        toApiCopyright(article.copyright),
        article.requiredLibraries.map(toApiRequiredLibrary),
        article.articleType,
        lang
      )
    }

    def toApiUpdateArticle(article: Article, lang: String, revision: Int): api.UpdateArticle = {
      api.UpdateArticle(
        revision,
        lang,
        findByLanguageOrBestEffort(article.title, lang).map(_.value),
        findByLanguageOrBestEffort(article.content, lang).map(_.value),
        findByLanguageOrBestEffort(article.tags, lang).map(_.value).getOrElse(Seq.empty),
        findByLanguageOrBestEffort(article.introduction, lang).map(_.value),
        findByLanguageOrBestEffort(article.metaDescription, lang).map(_.value),
        article.metaImageId,
        findByLanguageOrBestEffort(article.visualElement, lang).map(_.value),
        Some(toApiCopyright(article.copyright)),
        article.requiredLibraries.map(toApiRequiredLibrary),
        Some(article.articleType)
      )
    }

  }
}
