/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.model.domain

import java.net.URL
import java.util.Date

import scala.language.implicitConversions
import com.netaporter.uri.dsl._
import no.ndla.articleimport.integration.{LanguageContent, MigrationEditorialKeywords}

case class NodeGeneralContent(nid: String, tnid: String, title: String, content: String, language: String) {
  def isTranslation = !isMainNode

  def isMainNode = nid == tnid || tnid == "0"

  def asContentTitle = ArticleTitle(title, language)
}

case class NodeToConvert(titles: Seq[ArticleTitle],
                         contents: Seq[LanguageContent],
                         license: Option[String],
                         authors: Seq[Author],
                         tags: Seq[ArticleTag],
                         nodeType: String,
                         contentType: String,
                         created: Date,
                         updated: Date,
                         articleType: ArticleType.Value,
                         editorialKeywords: Seq[MigrationEditorialKeywords]) {

  def getMainNid: Option[String] = contents.find(_.isMainNode).map(_.nid)

  def getNid: Option[String] = contents.headOption.map(_.nid)

}

case class NodeWithConvertedMeta(titles: Seq[ArticleTitle],
                                 contents: Seq[LanguageContent],
                                 license: String,
                                 authors: Seq[Author],
                                 tags: Seq[ArticleTag],
                                 nodeType: String,
                                 contentType: String,
                                 created: Date,
                                 updated: Date,
                                 metaImages: Seq[ArticleMetaImage],
                                 articleType: ArticleType.Value,
                                 editorialKeywords: Seq[MigrationEditorialKeywords])

case class ContentFilMeta(nid: String,
                          tnid: String,
                          title: String,
                          fileName: String,
                          url: URL,
                          mimeType: String,
                          fileSize: String)

object ContentFilMeta {
  implicit def stringToUrl(s: String): URL = new URL(s.uri)
}

case class BiblioMeta(biblio: Biblio, authors: Seq[BiblioAuthor])

case class Biblio(title: String, bibType: String, year: String, edition: String, publisher: String)

case class BiblioAuthor(name: String, lastname: String, firstname: String)
