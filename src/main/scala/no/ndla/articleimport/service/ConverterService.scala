/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.articleimport.service

import com.typesafe.scalalogging.LazyLogging
import no.ndla.articleimport.ArticleImportProperties._
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
  this: ConverterModules with ExtractConvertStoreContent with ImageApiClient with Clock =>
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
        "content-import-client",
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

    private def toDomainCopyright(license: String, authors: Seq[Author]): Copyright = {
      val origin = authors.find(author => author.`type`.toLowerCase == "opphavsmann").map(_.name).getOrElse("")
      val authorsExcludingOrigin = authors.filterNot(x => x.name != origin && x.`type` == "opphavsmann")
      Copyright(license, origin, authorsExcludingOrigin)
    }

    def toDomainTitle(articleTitle: api.ArticleTitle): ArticleTitle = {
      ArticleTitle(articleTitle.title, articleTitle.language)
    }

    def toDomainContent(articleContent: api.ArticleContentV2): ArticleContent = {
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

   def toDomainCopyright(copyright: api.Copyright): Copyright = {
      Copyright(copyright.license.license, copyright.origin, copyright.authors.map(toDomainAuthor))
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

    def toApiArticleContentV2(content: ArticleContent): api.ArticleContentV2 = {
      api.ArticleContentV2(
        content.content,
        content.language
      )
    }

    def toApiCopyright(copyright: Copyright): api.Copyright = {
      api.Copyright(
        toApiLicense(copyright.license),
        copyright.origin,
        copyright.authors.map(toApiAuthor)
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

    def toApiConceptTitle(title: ConceptTitle): api.ConceptTitle = api.ConceptTitle(title.title, title.language)

    def toApiConceptContent(title: ConceptContent): api.ConceptContent= api.ConceptContent(title.content, title.language)

  }
}
