/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.model.domain

case class FootNoteItem(title: String,
                        `type`: String,
                        year: String,
                        edition: String,
                        publisher: String,
                        authors: Seq[String])

object FootNoteItem {

  def apply(biblio: Biblio, authors: Seq[BiblioAuthor]): FootNoteItem =
    FootNoteItem(biblio.title, biblio.bibType, biblio.year, biblio.edition, biblio.publisher, authors.map(x => x.name))
}
