/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters.contentbrowser

import no.ndla.articleimport.ArticleImportProperties._
import no.ndla.articleimport.{TestData, TestEnvironment, UnitSuite}
import no.ndla.articleimport.integration.{ImageCopyright, ImageLicense, ImageMetaInformation, ImageTag}
import no.ndla.articleimport.model.api.ImportException
import no.ndla.validation.EmbedTagRules.ResourceHtmlEmbedTag
import no.ndla.articleimport.model.domain._
import org.mockito.Mockito._
import org.mockito.ArgumentMatchers._

import scala.util.Success

class ContentBrowserConverterTest extends UnitSuite with TestEnvironment {
  val nodeId = "1234"
  val sampleAlt = "Fotografi"

  val sampleContentString =
    s"[contentbrowser ==nid=$nodeId==imagecache=Fullbredde==width===alt=$sampleAlt==link===node_link=1==link_type=link_to_content==lightbox_size===remove_fields[76661]=1==remove_fields[76663]=1==remove_fields[76664]=1==remove_fields[76666]=1==insertion=inline==link_title_text= ==link_text= ==text_align===css_class=contentbrowser contentbrowser]"
  val sampleContent = TestData.sampleContent.copy(content = s"<article>$sampleContentString</article>")

  override def beforeEach(): Unit = {
    when(extractService.getNodeData(any[String])).thenReturn(Success(TestData.sampleNodeToConvert))
  }

  test("contentbrowser strings of unsupported causes a Success to be returned with an error in ImportStatus") {
    val expectedResult =
      s"""<article><$ResourceHtmlEmbedTag data-message="Innhold mangler." data-resource="error"></article>"""

    when(extractService.getNodeType(nodeId))
      .thenReturn(Some("unsupported type"))

    val Success((result, status)) = contentBrowserConverter.convert(sampleContent, ImportStatus.empty)

    status.errors.size should be(1)
    status.errors should be(
      Seq(
        ImportException(
          s"$nodeId",
          "ContentBrowserConverter failed",
          Some(ImportException(s"$nodeId", s"Unsupported content unsupported type in node with id $nodeId")))))
    result.content should be(expectedResult)
  }

  test("That Content-browser strings of type image are converted into HTML img tags") {
    val (nodeId, imageUrl, alt) = ("1234", "full.jpeg", "Fotografi")
    val newId = "1"
    val imageMeta = ImageMetaInformation(newId,
                                         None,
                                         None,
                                         imageUrl,
                                         256,
                                         "",
                                         ImageCopyright(ImageLicense("", "", Some("")), "", List()),
                                         ImageTag(List(), ""))
    val expectedResult =
      s"""|<article>
          |<$ResourceHtmlEmbedTag data-align="" data-alt="$alt" data-caption="" data-resource="image" data-resource_id="1" data-size="full">
          |</article>""".stripMargin.replace("\n", "")

    when(extractService.getNodeType(nodeId)).thenReturn(Some("image"))
    when(imageApiClient.importImage(nodeId)).thenReturn(Some(imageMeta))
    val Success((result, _)) =
      contentBrowserConverter.convert(sampleContent, ImportStatus.empty)

    result.content should equal(expectedResult)
    result.requiredLibraries.size should equal(0)
  }

  test("That Content-browser strings of type oppgave are converted into content") {
    val contentTitle = "Oppgave title"
    val content =
      """<div class="paragraph">   Very important oppgave text  </div>"""
    val oppgave =
      NodeGeneralContent(nodeId, nodeId, contentTitle, content, "en")
    val expectedResult = s"""<article>$content</article>"""

    when(extractService.getNodeType(nodeId)).thenReturn(Some("oppgave"))
    when(extractService.getNodeGeneralContent(nodeId)).thenReturn(List(oppgave))
    val Success((result, _)) =
      contentBrowserConverter.convert(sampleContent, ImportStatus.empty)

    result.content should equal(expectedResult)
  }

  test("That Content-browser strings of type fagstoff are converted into content") {
    val contentTitle = "Fasgtoff title"
    val content =
      """<div class="paragraph">   Very important fagstoff text  </div>"""
    val oppgave =
      NodeGeneralContent(nodeId, nodeId, contentTitle, content, "en")
    val expectedResult = s"""<article>$content</article>"""

    when(extractService.getNodeType(nodeId)).thenReturn(Some("fagstoff"))
    when(extractService.getNodeGeneralContent(nodeId)).thenReturn(List(oppgave))
    val Success((result, _)) =
      contentBrowserConverter.convert(sampleContent, ImportStatus.empty)

    result.content should equal(expectedResult)
  }

  test("That Content-browser strings of type aktualitet are converted into content") {
    val contentTitle = "Aktualitet title"
    val content =
      """<div class="paragraph">   Very important aktualitet text  </div>"""
    val oppgave =
      NodeGeneralContent(nodeId, nodeId, contentTitle, content, "en")
    val expectedResult = s"""<article>$content</article>"""

    when(extractService.getNodeType(nodeId)).thenReturn(Some("aktualitet"))
    when(extractService.getNodeGeneralContent(nodeId)).thenReturn(List(oppgave))
    val Success((result, _)) =
      contentBrowserConverter.convert(sampleContent, ImportStatus.empty)

    result.content should equal(expectedResult)
  }

  test("That Content-browser strings of type veiledning are converted into content") {
    val contentTitle = "Veiledning title"
    val content =
      """<div class="paragraph">   Very important veiledning text  </div>"""
    val oppgave =
      NodeGeneralContent(nodeId, nodeId, contentTitle, content, "en")
    val expectedResult = s"""<article>$content</article>"""

    when(extractService.getNodeType(nodeId)).thenReturn(Some("veiledning"))
    when(extractService.getNodeGeneralContent(nodeId)).thenReturn(List(oppgave))
    val Success((result, _)) =
      contentBrowserConverter.convert(sampleContent, ImportStatus.empty)

    result.content should equal(expectedResult)
  }

  test("That Content-browser strings of type video are converted into HTML img tags") {
    val expectedResult =
      s"""<article><$ResourceHtmlEmbedTag data-account="$NDLABrightcoveAccountId" data-caption="" data-player="$NDLABrightcovePlayerId" data-resource="brightcove" data-videoid="ref:$nodeId"></article>"""
    when(extractService.getNodeType(nodeId)).thenReturn(Some("video"))
    val Success((result, _)) =
      contentBrowserConverter.convert(sampleContent, ImportStatus.empty)
    val strippedResult =
      " +".r.replaceAllIn(result.content.replace("\n", ""), " ")

    strippedResult should equal(expectedResult)
    result.requiredLibraries.size should equal(0)
  }

  test("That content-browser strings of type biblio are converted into content") {
    val initialContent =
      sampleContent.copy(content = s"""<article>$sampleContentString</a><h1>CONTENT</h1>more content</article>""")
    val biblio =
      BiblioMeta(Biblio("title", "book", "2009", "1", "me"), Seq(BiblioAuthor("first last", "last", "first")))
    val expectedResult =
      s"""<article><$ResourceHtmlEmbedTag data-authors="${biblio.authors.head.name}" data-edition="${biblio.biblio.edition}" data-publisher="${biblio.biblio.publisher}" data-resource="footnote" data-title="${biblio.biblio.title}" data-type="${biblio.biblio.bibType}" data-year="${biblio.biblio.year}"><h1>CONTENT</h1>more content</article>"""

    when(extractService.getNodeType(nodeId)).thenReturn(Some("biblio"))
    when(extractService.getBiblioMeta(nodeId)).thenReturn(Some(biblio))
    val Success((result, status)) =
      contentBrowserConverter.convert(initialContent, ImportStatus.empty)
    val strippedContent = " +".r.replaceAllIn(result.content, " ")

    strippedContent should equal(expectedResult)
    status.messages.isEmpty should be(true)
  }

  test("meta description is converted") {
    val metaDescription =
      """<div class="paragraph">   Very important aktualitet text  </div>"""
    val oppgave = NodeGeneralContent(nodeId, nodeId, "Aktualitet title", metaDescription, "en")
    val expectedResult = s"""<article>$metaDescription</article>"""

    when(extractService.getNodeType(nodeId)).thenReturn(Some("aktualitet"))
    when(extractService.getNodeGeneralContent(nodeId)).thenReturn(List(oppgave))
    val Success((result, _)) =
      contentBrowserConverter.convert(sampleContent.copy(content = "", metaDescription = sampleContent.content),
                                      ImportStatus.empty)

    result.content should equal("")
    result.metaDescription should equal(expectedResult)
  }
}
