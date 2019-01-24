/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.model.domain

import no.ndla.articleimport.model.api.ImportException

case class ImportStatus(messages: Seq[String],
                        errors: Seq[ImportException],
                        visitedNodes: Set[String] = Set.empty,
                        articleId: Option[Long] = None,
                        forceUpdateArticles: Boolean = false,
                        nodeLocalContext: NodeLocalImportStatus = NodeLocalImportStatus(),
                        importId: Option[String] = None,
                        articleType: ArticleType.Value = ArticleType.Standard) {

  def addMessage(message: String): ImportStatus =
    this.copy(messages = this.messages :+ message)

  def addMessages(messages: Seq[String]): ImportStatus =
    this.copy(messages = this.messages ++ messages)

  def addErrors(errorMessages: Seq[ImportException]): ImportStatus =
    this.copy(errors = this.errors ++ errorMessages)

  def addError(errorMessage: ImportException): ImportStatus =
    this.copy(errors = this.errors :+ errorMessage)

  def addVisitedNode(nodeID: String): ImportStatus =
    this.copy(visitedNodes = this.visitedNodes + nodeID)

  def addVisitedNodes(nodeIDs: Set[String]): ImportStatus =
    this.copy(visitedNodes = this.visitedNodes ++ nodeIDs)

  def setArticleId(id: Long): ImportStatus = this.copy(articleId = Some(id))

  def withNewNodeLocalContext(): ImportStatus =
    this.copy(nodeLocalContext = NodeLocalImportStatus(List.empty, List.empty, this.nodeLocalContext.depth + 1))

  def resetNodeLocalContext(originalContext: NodeLocalImportStatus): ImportStatus =
    this.copy(nodeLocalContext = originalContext)

  def addInsertedLicense(license: Option[String]): ImportStatus =
    this.copy(nodeLocalContext = this.nodeLocalContext.addInsertedLicense(license))

  def addInsertedAuthors(authors: List[Author]): ImportStatus =
    this.copy(nodeLocalContext = this.nodeLocalContext.addInsertedAuthors(authors))

  def withArticleType(articleType: ArticleType.Value): ImportStatus = this.copy(articleType = articleType)
}

object ImportStatus {
  def empty = ImportStatus(Seq.empty, Seq.empty, Set.empty, None)

  def empty(forceUpdate: Boolean = false, importId: Option[String] = None): ImportStatus = {
    ImportStatus(Seq.empty, Seq.empty, Set.empty, None, forceUpdateArticles = forceUpdate, importId = importId)
  }
}

case class NodeLocalImportStatus(insertedLicenses: List[String] = List.empty,
                                 insertedAuthors: List[Author] = List.empty,
                                 depth: Int = 0) {

  def addInsertedLicense(license: Option[String]): NodeLocalImportStatus = {
    this.copy(insertedLicenses = this.insertedLicenses ++ license.toList)
  }

  def addInsertedAuthors(authors: List[Author]): NodeLocalImportStatus = {
    this.copy(insertedAuthors = this.insertedAuthors ++ authors)
  }
}
