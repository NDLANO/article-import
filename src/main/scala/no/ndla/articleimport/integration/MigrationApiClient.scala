/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.integration

import java.net.URL
import java.util.Date

import no.ndla.articleimport.ArticleImportProperties.{Environment, MigrationHost, MigrationPassword, MigrationUser}
import no.ndla.articleimport.model.domain._
import no.ndla.articleimport.service.TagsService
import no.ndla.network.NdlaClient

import scala.util.Try
import scalaj.http.Http
import io.lemonlabs.uri.dsl._
import no.ndla.articleimport.caching.Memoize

trait MigrationApiClient {
  this: NdlaClient with TagsService =>
  val migrationApiClient: MigrationApiClient

  class MigrationApiClient {
    val DBSource = "red"
    private val ContentMigrationBaseEndpoint = s"$MigrationHost/contents"
    private val ContentDataEndpoint = s"$ContentMigrationBaseEndpoint/:node_id" ? (s"db-source" -> s"$DBSource")
    private val ContentTypeEndpoint = s"$ContentMigrationBaseEndpoint/type/:node_id" ? (s"db-source" -> s"$DBSource")
    private val ContentEmbedEndpoint = s"$ContentMigrationBaseEndpoint/embedmeta/:node_id" ? (s"db-source" -> s"$DBSource")
    private val ContentFileEndpoint = s"$ContentMigrationBaseEndpoint/filemeta/:node_id" ? (s"db-source" -> s"$DBSource")
    private val ContentGeneralEndpoint = s"$ContentMigrationBaseEndpoint/generalcontent/:node_id" ? (s"db-source" -> s"$DBSource")
    private val ContentBiblioMetaEndpoint = s"$ContentMigrationBaseEndpoint/bibliometa/:node_id" ? (s"db-source" -> s"$DBSource")
    private val ContentSubjectMetaEndpoint = s"$ContentMigrationBaseEndpoint/subjectfornode/:node_id" ? (s"db-source" -> s"$DBSource")

    def getContentNodeData(nodeId: String): Try[MigrationMainNodeImport] =
      get[MigrationMainNodeImport](ContentDataEndpoint, nodeId)

    private def get[A](endpointUrl: String, nodeId: String)(implicit mf: Manifest[A]): Try[A] = {
      ndlaClient.fetchWithBasicAuth[A](Http(endpointUrl.replace(":node_id", nodeId))
                                         .timeout(1000 * 30, 1000 * 30),
                                       MigrationUser,
                                       MigrationPassword)
    }

    lazy val getContentType: Memoize[String, Try[MigrationNodeType]] =
      Memoize(get[MigrationNodeType](ContentTypeEndpoint, _))

    lazy val getNodeEmbedData: Memoize[String, Try[MigrationEmbedMeta]] = Memoize(
      get[MigrationEmbedMeta](ContentEmbedEndpoint, _))

    lazy val getFilMeta: Memoize[String, Try[Seq[MigrationContentFileMeta]]] = Memoize(
      get[Seq[MigrationContentFileMeta]](ContentFileEndpoint, _))

    lazy val getNodeGeneralContent: Memoize[String, Try[Seq[MigrationNodeGeneralContent]]] = Memoize(
      get[Seq[MigrationNodeGeneralContent]](ContentGeneralEndpoint, _))

    lazy val getBiblioMeta: Memoize[String, Try[MigrationContentBiblioMeta]] = Memoize(
      get[MigrationContentBiblioMeta](ContentBiblioMetaEndpoint, _))

    lazy val getSubjectForNode: Memoize[String, Try[Set[MigrationSubjectMeta]]] =
      Memoize((nodeId: String) => get[Seq[MigrationSubjectMeta]](ContentSubjectMetaEndpoint, nodeId).map(_.toSet))

    private val getAllNodeTranslationNids: Memoize[String, Try[Set[String]]] =
      Memoize((nodeId: String) => getContentNodeData(nodeId).map(_.contents.map(_.nid).toSet))

    def getAllTranslationNids(nodeId: String): Try[Set[String]] =
      getAllNodeTranslationNids(nodeId)
  }
}

case class MigrationMainNodeImport(titles: Seq[MigrationContentTitle],
                                   ingresses: Seq[MigrationIngress],
                                   contents: Seq[MigrationContent],
                                   authors: Seq[MigrationContentAuthor],
                                   license: Option[String],
                                   nodeType: Option[String],
                                   pageTitles: Seq[MigrationPageTitle],
                                   visualElements: Seq[MigrationVisualElement],
                                   relatedContents: Seq[MigrationRelatedContents],
                                   editorialKeywords: Seq[MigrationEditorialKeywords],
                                   learningResourceType: Seq[MigrationLearningResourceType],
                                   difficulty: Seq[MigrationDifficulty],
                                   contentType: Seq[MigrationContentType],
                                   innholdAndFag: Seq[MigrationInnholdsKategoriAndFag],
                                   fagressurs: Seq[MigrationFagressurs],
                                   emneartikkelData: Seq[MigrationEmneArtikkelData]) {

  def asNodeToConvert(nodeId: String, tags: List[ArticleTag]): NodeToConvert = {
    val articleType = nodeType
      .map(
        nType =>
          if (nType == "emneartikkel") ArticleType.TopicArticle
          else ArticleType.Standard)
      .getOrElse(ArticleType.Standard)

    val licenseMapping = Map(
      "by" -> "CC-BY-4.0",
      "by-sa" -> "CC-BY-SA-4.0",
      "by-nc" -> "CC-BY-NC-4.0",
      "by-nd" -> "CC-BY-ND-4.0",
      "by-nc-sa" -> "CC-BY-NC-SA-4.0",
      "by-nc-nd" -> "CC-BY-NC-ND-4.0",
      "by-3.0" -> "CC-BY-4.0",
      "by-sa-3.0" -> "CC-BY-SA-4.0",
      "by-nc-3.0" -> "CC-BY-NC-4.0",
      "by-nd-3.0" -> "CC-BY-ND-4.0",
      "by-nc-sa-3.0" -> "CC-BY-NC-SA-4.0",
      "by-nc-nd-3.0" -> "CC-BY-NC-ND-4.0",
      "copyrighted" -> "COPYRIGHTED",
      "cc0" -> "CC0-1.0",
      "pd" -> "PD",
      "nolaw" -> "CC0-1.0",
      "noc" -> "PD"
    )
    val lic = emptySomeToNone(license).flatMap(licenseMapping.get)

    val languageContents = asLanguageContents.map(c =>
      c.copy(visualElement = if (articleType == ArticleType.TopicArticle) c.visualElement else None))

    NodeToConvert(
      titles.map(x => x.asContentTitle),
      languageContents,
      lic,
      authors.flatMap(x => x.asAuthor),
      tags,
      nodeType.getOrElse("unknown"),
      contentType.headOption.map(_.`type`).getOrElse("unknown"),
      contents.minBy(_.created).created,
      contents.maxBy(_.changed).changed,
      articleType,
      editorialKeywords
    )
  }

  def asLanguageContents: Seq[LanguageContent] = {
    contents.map(content => {
      val relatedContent = relatedContents
        .find(_.language == content.language)
        .map(_.related)
        .getOrElse(Seq.empty)

      LanguageContent(
        content.nid,
        content.tnid,
        content.content,
        getMetaDescription(content),
        Language.languageOrUnknown(content.language),
        visualElements.find(_.language == content.language).map(_.element),
        nodeType.getOrElse("unknown"),
        titles.find(_.language == content.language).map(_.title),
        relatedContent,
        getMetaImage(content.language),
        ingress = getIngress(content.language)
      )
    })
  }

  private def getIngress(language: Option[String]): Option[LanguageIngress] = {
    getEmneArtikkel(language) match {
      case Some(data) => Option(LanguageIngress(data.ingress, None))
      case None =>
        ingresses
          .find(ingress => ingress.language == language && ingress.ingressVisPaaSiden == 1)
          .map(ingress => LanguageIngress(ingress.content.getOrElse(""), ingress.imageNid))
    }
  }

  private def getMetaDescription(content: MigrationContent): String = {
    getEmneArtikkel(content.language) match {
      case Some(data) => data.metaDescription
      case None =>
        ingresses
          .find(_.language == content.language)
          .flatMap(_.content)
          .getOrElse(content.metaDescription)
    }
  }

  private def getMetaImage(language: Option[String]): Option[String] =
    ingresses.find(ing => ing.language == language).flatMap(_.imageNid)

  private def getEmneArtikkel(language: Option[String]) =
    emneartikkelData.find(_.language == language)
}

case class MigrationNodeGeneralContent(nid: String, tnid: String, title: String, content: String, language: String) {

  def asNodeGeneralContent: NodeGeneralContent =
    NodeGeneralContent(nid, tnid, title, content, language)
}

case class MigrationContentAuthor(`type`: Option[String], name: Option[String]) {

  def asAuthor: Option[Author] = {
    (`type`, name) match {
      case (None, None) => None
      case (authorType, authorName) =>
        Some(Author(authorType.getOrElse(""), authorName.getOrElse("")))
    }
  }
}

case class MigrationContentTitle(title: String, language: Option[String]) {

  def asContentTitle: ArticleTitle =
    ArticleTitle(title, Language.languageOrUnknown(language))
}

case class MigrationIngress(nid: String,
                            content: Option[String],
                            imageNid: Option[String],
                            ingressVisPaaSiden: Int,
                            language: Option[String])

case class MigrationContent(nid: String,
                            tnid: String,
                            content: String,
                            metaDescription: String,
                            language: Option[String],
                            created: Date,
                            changed: Date)

case class MigrationNodeType(nodeType: String)

case class MigrationContentBiblioMeta(biblio: MigrationBiblio, authors: Seq[MigrationBiblioAuthor]) {

  def asBiblioMeta: BiblioMeta =
    BiblioMeta(biblio.asBiblio, authors.map(x => x.asBiblioAuthor))
}

case class MigrationBiblio(title: String, bibType: String, year: String, edition: String, publisher: String) {
  def asBiblio: Biblio = Biblio(title, bibType, year, edition, publisher)
}

case class MigrationBiblioAuthor(name: String, lastname: String, firstname: String) {
  def asBiblioAuthor: BiblioAuthor = BiblioAuthor(name, lastname, firstname)
}

case class MigrationContentFileMeta(nid: String,
                                    tnid: String,
                                    title: String,
                                    fileName: String,
                                    url: String,
                                    mimeType: String,
                                    fileSize: String) {

  def asContentFilMeta: ContentFilMeta =
    ContentFilMeta(nid, tnid, title, fileName, url.withScheme("https"), mimeType, fileSize)
}

case class MigrationEmbedMeta(url: Option[String], embedCode: Option[String])

case class MigrationPageTitle(title: String, `type`: String, language: Option[String])

case class MigrationVisualElement(element: String, `type`: String, language: Option[String])

case class MigrationRelatedContents(related: Seq[MigrationRelatedContent], language: Option[String])

case class MigrationRelatedContent(nid: String, title: String, uri: String, fagligRelation: Int)

case class MigrationEditorialKeywords(keywords: Seq[String], language: Option[String])

case class MigrationLearningResourceType(resourceType: String, language: Option[String])

case class MigrationDifficulty(difficulty: String, language: Option[String])

case class MigrationContentType(`type`: String, language: Option[String])

case class MigrationInnholdsKategoriAndFag(innhold: String, fag: String, language: Option[String])

case class MigrationFagressurs(fagressursType: String, velgFagressurs: String, language: Option[String])

case class MigrationSubjectMeta(nid: String, title: String)

case class MigrationEmneArtikkelData(ingress: String, metaDescription: String, language: Option[String])
