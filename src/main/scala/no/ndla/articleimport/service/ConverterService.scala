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
import no.ndla.articleimport.integration.{ImageApiClient, LanguageIngress, MigrationApiClient}
import no.ndla.articleimport.model.api
import no.ndla.articleimport.model.api.{ImportException, ImportExceptions}
import no.ndla.articleimport.model.domain.Language._
import no.ndla.articleimport.model.domain._
import no.ndla.articleimport.service.converters.MetaInfoConverter
import no.ndla.mapping.License.getLicense
import no.ndla.validation.{EmbedTagRules, HtmlTagRules, ResourceType, TagAttributes}

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

trait ConverterService {
  this: ConverterModules
    with ExtractConvertStoreContent
    with ImageApiClient
    with Clock
    with User
    with MetaInfoConverter
    with MigrationApiClient =>
  val converterService: ConverterService

  class ConverterService extends LazyLogging {

    def toDomainArticle(nodeToConvert: NodeToConvert, importStatus: ImportStatus): Try[(Content, ImportStatus)] = {
      val nodeIdsToImport = nodeToConvert.contents.map(_.nid).toSet

      convert(nodeToConvert, maxConvertionRounds, importStatus.addVisitedNodes(nodeIdsToImport))
        .flatMap { case (content, status) => postProcess(content, status) } match {
        case Failure(f) =>
          Failure(f)
        case Success((convertedContent, converterStatus)) if convertedContent.nodeType == nodeTypeBegrep =>
          Success((toDomainConcept(convertedContent), converterStatus))
        case Success((convertedContent, converterStatus)) =>
          MetaInfoConverter.convert(convertedContent, converterStatus).map {
            case (converted, is) => (toDomainArticle(converted), is)
          }
      }
    }

    @tailrec private def convert(nodeToConvert: NodeToConvert,
                                 maxRoundsLeft: Int,
                                 importStatus: ImportStatus): Try[(NodeToConvert, ImportStatus)] = {
      if (maxRoundsLeft == 0) {
        val message =
          "Maximum number of converter rounds reached; Some content might not be converted"
        logger.warn(message)
        return Success((nodeToConvert, importStatus.copy(messages = importStatus.messages :+ message)))
      }

      val (updatedContent, updatedStatus) =
        executeConverterModules(nodeToConvert, importStatus) match {
          case Failure(e) => return Failure(e)
          case Success(s) => s
        }

      // If this converting round did not yield any changes to the content, this node is finished (case true)
      // If changes were made during this conversion, we run the converters again (case false)
      updatedContent == nodeToConvert match {
        case true  => Success((updatedContent, updatedStatus))
        case false => convert(updatedContent, maxRoundsLeft - 1, updatedStatus)
      }
    }

    private def postProcess(nodeToConvert: NodeToConvert,
                            importStatus: ImportStatus): Try[(NodeToConvert, ImportStatus)] =
      executePostprocessorModules(nodeToConvert, importStatus)

    private[service] def toDomainArticle(convertedNode: NodeWithConvertedMeta): Article = {
      val requiredLibraries =
        convertedNode.contents.flatMap(_.requiredLibraries).distinct
      val ingresses =
        convertedNode.contents.flatMap(content => content.asArticleIntroduction)
      val visualElements = convertedNode.contents.flatMap(_.asVisualElement)

      val languagesInNode: Set[String] =
        (convertedNode.titles.map(_.language) ++
          convertedNode.contents.map(_.language) ++
          ingresses.map(_.language)).toSet

      Article(
        None,
        None,
        convertedNode.titles,
        convertedNode.contents.map(_.asContent),
        toDomainCopyright(convertedNode.license, convertedNode.authors),
        convertedNode.tags.filter(tag => languagesInNode.contains(tag.language)),
        requiredLibraries,
        visualElements,
        ingresses,
        convertedNode.contents.map(_.asArticleMetaDescription),
        convertedNode.metaImages,
        convertedNode.created,
        convertedNode.updated,
        authUser.userOrClientid(),
        convertedNode.articleType.toString,
        convertedNode.editorialKeywords.flatMap(_.keywords)
      )
    }

    private def toDomainConcept(convertedNode: NodeToConvert): Concept = {
      val license = Option(convertedNode.license).filter(_.nonEmpty)

      Concept(
        None,
        convertedNode.titles.map(title => ConceptTitle(title.title, title.language)),
        convertedNode.contents.map(content => ConceptContent(content.content, content.language)),
        license.map(l => toDomainCopyright(l.getOrElse(""), convertedNode.authors)),
        convertedNode.created,
        convertedNode.updated
      )
    }

    private def toDomainAuthor(author: Author): Author = {
      val creatorMap =
        (oldCreatorTypes zip creatorTypes).toMap.withDefaultValue(None)
      val processorMap =
        (oldProcessorTypes zip processorTypes).toMap.withDefaultValue(None)
      val rightsholderMap = (oldRightsholderTypes zip rightsholderTypes).toMap
        .withDefaultValue(None)

      (creatorMap(author.`type`.toLowerCase),
       processorMap(author.`type`.toLowerCase),
       rightsholderMap(author.`type`.toLowerCase)) match {
        case (t: String, _, _) => Author(t.capitalize, author.name)
        case (_, t: String, _) => Author(t.capitalize, author.name)
        case (_, _, t: String) => Author(t.capitalize, author.name)
        case (_, _, _)         => Author(author.`type`, author.name)
      }
    }

    private[service] def toDomainCopyright(license: String, authors: Seq[Author]): Copyright = {
      val origin = authors
        .find(author => author.`type`.toLowerCase == "opphavsmann")
        .map(_.name)
      val creators = authors
        .filter(a => oldCreatorTypes.contains(a.`type`.toLowerCase))
        .map(toDomainAuthor)
      val processors = authors
        .filter(a => oldProcessorTypes.contains(a.`type`.toLowerCase))
        .map(toDomainAuthor)
      val rightsholders = authors
        .filter(a => oldRightsholderTypes.contains(a.`type`.toLowerCase))
        .map(toDomainAuthor)

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

    def toDomainMetaDescriptionV2(meta: Option[String], language: String): Seq[ArticleMetaDescription] = {
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
      document
        .select("embed")
        .asScala
        .map(el => {
          ResourceType
            .valueOf(el.attr(TagAttributes.DataResource.toString))
            .map(EmbedTagRules.attributesForResourceType)
            .map(knownAttributes => HtmlTagRules.removeIllegalAttributes(el, knownAttributes.all.map(_.toString)))
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
        case Some(l) =>
          api.License(l.license, Option(l.description), l.url)
        case None =>
          api.License("unknown", None, None)
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

    def toApiArticleMetaDescription(metaDescription: ArticleMetaDescription): api.ArticleMetaDescription = {
      api.ArticleMetaDescription(metaDescription.content, metaDescription.language)
    }

    def createLinkToOldNdla(nodeId: String): String =
      s"//red.ndla.no/node/$nodeId"

    def toNewApiConcept(concept: Concept, language: String): api.NewConcept = {
      val title = findByLanguageOrBestEffort(concept.title, language)
        .map(_.title)
        .getOrElse("")
      val content = findByLanguageOrBestEffort(concept.content, language)
        .map(_.content)
        .getOrElse("")

      api.NewConcept(
        language,
        title,
        content,
        concept.copyright.map(toApiCopyright)
      )
    }

    def toUpdateApiConcept(concept: Concept, language: String): api.UpdateConcept = {
      val title =
        findByLanguageOrBestEffort(concept.title, language).map(_.title)
      val content =
        findByLanguageOrBestEffort(concept.content, language).map(_.content)

      api.UpdateConcept(
        language,
        title,
        content,
        concept.copyright.map(toApiCopyright)
      )
    }

    def toApiConceptTitle(title: ConceptTitle): api.ConceptTitle =
      api.ConceptTitle(title.title, title.language)

    def toApiConceptContent(title: ConceptContent): api.ConceptContent =
      api.ConceptContent(title.content, title.language)

    def toApiNewArticle(article: Article, lang: String): api.NewArticle = {
      api.NewArticle(
        findByLanguageOrBestEffort(article.title, lang)
          .map(_.title)
          .getOrElse(""),
        findByLanguageOrBestEffort(article.content, lang)
          .map(_.content)
          .getOrElse(""),
        findByLanguageOrBestEffort(article.tags, lang)
          .map(_.tags)
          .getOrElse(Seq.empty),
        findByLanguageOrBestEffort(article.introduction, lang).map(_.introduction),
        findByLanguageOrBestEffort(article.metaDescription, lang).map(_.content),
        findByLanguageOrBestEffort(article.metaImageId, lang).map(_.imageId), // TODO: AltText
        findByLanguageOrBestEffort(article.visualElement, lang).map(_.resource),
        toApiCopyright(article.copyright),
        article.requiredLibraries.map(toApiRequiredLibrary),
        article.articleType,
        article.editorialKeywords,
        lang
      )
    }

    def toApiUpdateArticle(article: Article, lang: String, revision: Int): api.UpdateArticle = {
      api.UpdateArticle(
        revision,
        lang,
        findByLanguageOrBestEffort(article.title, lang).map(_.title),
        findByLanguageOrBestEffort(article.content, lang).map(_.content),
        findByLanguageOrBestEffort(article.tags, lang)
          .map(_.tags)
          .getOrElse(Seq.empty),
        findByLanguageOrBestEffort(article.introduction, lang).map(_.introduction),
        findByLanguageOrBestEffort(article.metaDescription, lang).map(_.content),
        findByLanguageOrBestEffort(article.metaImageId, lang).map(_.imageId), // TODO: AltText
        findByLanguageOrBestEffort(article.visualElement, lang).map(_.resource),
        Some(toApiCopyright(article.copyright)),
        article.requiredLibraries.map(toApiRequiredLibrary),
        article.editorialKeywords,
        Some(article.articleType)
      )
    }

    def generateImportErrorMessage(ex: ImportExceptions): ImportError =
      ImportError(messages = getAllErrors(ex))

    def generateImportError(ex: ImportException): ImportError =
      ImportError(messages = Set(ImportMessages(Set.empty, Set(ex.message))))

    private def getAllErrors(exceptions: ImportExceptions): Set[ImportMessages] = {
      val mainException = ImportMessages(exceptions.failedNodeIds, exceptions.errors.map(_.getMessage).toSet)

      val importErrors = exceptions.errors.flatMap {
        case ex: ImportExceptions => getAllErrors(ex)
        case ex: ImportException  => getAllErrors(ex)
        case ex =>
          Seq(ImportMessages(exceptions.failedNodeIds, Set(ex.getMessage)))
      } ++ Seq(mainException)

      val keys = importErrors.map(_.nids).sortBy(k => -k.size)
      importErrors
        .groupBy(k => keys.find(a => k.nids.subsetOf(a)))
        .collect {
          case (Some(nids), errors) =>
            ImportMessages(nids, errors.flatMap(_.messages).toSet)
        }
        .toSet
    }

    private def getAllErrors(exception: ImportException): Set[ImportMessages] = {
      exception match {
        case ImportException(nid, msg, Some(ex: ImportExceptions)) =>
          val nids =
            migrationApiClient.getAllTranslationNids(nid).getOrElse(Set(nid))
          Set(ImportMessages(nids, Set(msg))) ++ getAllErrors(ex)
        case ImportException(nid, msg, Some(ex: ImportException)) =>
          val nids =
            migrationApiClient.getAllTranslationNids(nid).getOrElse(Set(nid))
          Set(ImportMessages(nids, Set(msg))) ++ getAllErrors(ex)
        case ImportException(nid, msg, exOpt) =>
          val nids =
            migrationApiClient.getAllTranslationNids(nid).getOrElse(Set(nid))
          Set(ImportMessages(nids, exOpt.map(_.getMessage).toSet ++ Set(msg)))
      }
    }

  }
}
