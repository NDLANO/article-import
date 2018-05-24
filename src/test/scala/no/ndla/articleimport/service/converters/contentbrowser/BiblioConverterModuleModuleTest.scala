/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters.contentbrowser

import no.ndla.articleimport.model.domain.{Biblio, BiblioAuthor, BiblioMeta, ImportStatus}
import no.ndla.articleimport.{TestData, TestEnvironment, UnitSuite}
import no.ndla.validation.EmbedTagRules.ResourceHtmlEmbedTag

import scala.util.Success
import org.mockito.Mockito._

class BiblioConverterModuleModuleTest extends UnitSuite with TestEnvironment {
  val nodeId = "1234"
  val altText = "Jente som spiser melom. Grønn bakgrunn, rød melon. Fotografi."

  val contentString =
    s"[contentbrowser ==nid=$nodeId==imagecache=Fullbredde==width===alt=$altText==link===node_link=1==link_type=link_to_content==lightbox_size===remove_fields[76661]=1==remove_fields[76663]=1==remove_fields[76664]=1==remove_fields[76666]=1==insertion===link_title_text= ==link_text= ==text_align===css_class=contentbrowser contentbrowser]"

  val (linkUrl, linkEmbedCode) =
    ("https://www.youtube.com/watch?v=1qN72LEQnaU",
     """<iframe src="https://www.youtube.com/embed/1qN72LEQnaU?feature=oembed"></iframe>""")

  test("That BiblioConverter replaces contentbrowser strings with an a tag containing the nodeId") {
    val insertion = "inline"
    val content =
      TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "alt" -> altText, "insertion" -> insertion)
    val biblio =
      BiblioMeta(Biblio("title", "book", "2009", "1", "me"), Seq(BiblioAuthor("first last", "last", "first")))
    val expectedResult =
      s"""<$ResourceHtmlEmbedTag data-authors="${biblio.authors.head.name}" data-edition="${biblio.biblio.edition}" data-publisher="${biblio.biblio.publisher}" data-resource="footnote" data-title="${biblio.biblio.title}" data-type="${biblio.biblio.bibType}" data-year="${biblio.biblio.year}" />"""

    when(extractService.getBiblioMeta(nodeId)).thenReturn(Some(biblio))
    val Success((result, requiredLibraries, importStatus)) =
      BiblioConverterModule.convert(content, ImportStatus.empty)

    result should equal(expectedResult)
    requiredLibraries.isEmpty should be(true)
    importStatus.messages.isEmpty should be(true)
  }

  test("Authors list should only contain unique names") {
    val insertion = "inline"
    val content =
      TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "alt" -> altText, "insertion" -> insertion)
    val author = BiblioAuthor("Henrik Henriksen", "Henriksen", "Henrik")
    val biblio = BiblioMeta(Biblio("title", "book", "2009", "1", "me"),
                            Seq(author, author, author, BiblioAuthor("first last", "last", "first")))
    val expectedResult =
      s"""<$ResourceHtmlEmbedTag data-authors="${biblio.authors.distinct
        .map(_.name)
        .mkString(";")}" data-edition="${biblio.biblio.edition}" data-publisher="${biblio.biblio.publisher}" data-resource="footnote" data-title="${biblio.biblio.title}" data-type="${biblio.biblio.bibType}" data-year="${biblio.biblio.year}" />"""

    when(extractService.getBiblioMeta(nodeId)).thenReturn(Some(biblio))
    val Success((result, requiredLibraries, importStatus)) =
      BiblioConverterModule.convert(content, ImportStatus.empty)

    result should equal(expectedResult)
    requiredLibraries.isEmpty should be(true)
    importStatus.messages.isEmpty should be(true)
  }

}
