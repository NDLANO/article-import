/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.model.domain

case class ImportStatus(messages: Seq[String],
                        visitedNodes: Set[String] = Set(),
                        articleId: Option[Long] = None,
                        importRelatedArticles: Boolean = true,
                        forceUpdateArticles: Boolean = false,
                        nodeLocalContext: NodeLocalImportStatus = NodeLocalImportStatus()) {

  def addMessage(message: String): ImportStatus =
    this.copy(messages = this.messages :+ message)

  def addMessages(messages: Seq[String]): ImportStatus =
    this.copy(messages = this.messages ++ messages)

  def addVisitedNode(nodeID: String): ImportStatus =
    this.copy(visitedNodes = this.visitedNodes + nodeID)

  def addVisitedNodes(nodeIDs: Set[String]): ImportStatus =
    this.copy(visitedNodes = this.visitedNodes ++ nodeIDs)
  def setArticleId(id: Long): ImportStatus = this.copy(articleId = Some(id))

  def withNewNodeLocalContext(): ImportStatus =
    this.copy(nodeLocalContext = NodeLocalImportStatus(List.empty, List.empty))

  def resetNodeLocalContext(originalContext: NodeLocalImportStatus): ImportStatus =
    this.copy(nodeLocalContext = originalContext)

  def addInsertedLicense(license: Option[String]): ImportStatus =
    this.copy(nodeLocalContext = this.nodeLocalContext.addInsertedLicense(license))

  def addInsertedAuthors(authors: List[Author]): ImportStatus =
    this.copy(nodeLocalContext = this.nodeLocalContext.addInsertedAuthors(authors))
}

object ImportStatus {
  def empty = ImportStatus(Seq.empty, Set.empty, None)

  def empty(forceUpdate: Boolean) =
    ImportStatus(Seq.empty, Set.empty, None, forceUpdateArticles = forceUpdate)

  def apply(message: String, visitedNodes: Set[String]): ImportStatus =
    ImportStatus(Seq(message), visitedNodes, None)

  def apply(importStatuses: Seq[ImportStatus]): ImportStatus = {
    val (messages, visitedNodes, articleIds) =
      importStatuses.map(x => (x.messages, x.visitedNodes, x.articleId)).unzip3
    ImportStatus(messages.flatten.distinct, visitedNodes.flatten.toSet, articleIds.lastOption.flatten)
  }

}

case class NodeLocalImportStatus(insertedLicenses: List[String] = List.empty,
                                 insertedAuthors: List[Author] = List.empty) {

  def addInsertedLicense(license: Option[String]): NodeLocalImportStatus = {
    this.copy(insertedLicenses = this.insertedLicenses ++ license.toList)
  }

  def addInsertedAuthors(authors: List[Author]): NodeLocalImportStatus = {
    this.copy(insertedAuthors = this.insertedAuthors ++ authors)
  }
}
