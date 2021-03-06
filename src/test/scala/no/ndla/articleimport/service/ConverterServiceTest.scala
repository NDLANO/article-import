/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service

import java.net.URL
import java.util.Date

import no.ndla.articleimport.ArticleImportProperties.Domain
import no.ndla.articleimport.integration.TaxonomyApiClient.Resource
import no.ndla.articleimport.model.domain.ContentFilMeta._
import no.ndla.articleimport.integration._
import no.ndla.articleimport.model.api
import no.ndla.articleimport.model.api.ImportException
import no.ndla.articleimport.model.domain._
import no.ndla.articleimport.service.converters.TableConverter
import no.ndla.validation.EmbedTagRules.ResourceHtmlEmbedTag
import no.ndla.articleimport.{TestData, TestEnvironment, UnitSuite}
import no.ndla.validation.ResourceType
import no.ndla.mapping.License.{CC_BY, CC_BY_SA}
import org.mockito.Mockito._
import org.mockito.ArgumentMatchers.{eq => eqTo, _}
import org.mockito.invocation.InvocationOnMock

import scala.util.{Success, Try}

class ConverterServiceTest extends UnitSuite with TestEnvironment {

  val service = new ConverterService
  val contentTitle = ArticleTitle("", "unknown")
  val author = Author("forfatter", "Henrik")
  val tag = ArticleTag(List("asdf"), "nb")
  val requiredLibrary = RequiredLibrary("", "", "")
  val nodeId = "1234"
  val sampleAlt = "Fotografi"

  val sampleContentString =
    s"[contentbrowser ==nid=$nodeId==imagecache=Fullbredde==width===alt=$sampleAlt==link===node_link=1==link_type=link_to_content==lightbox_size===remove_fields[76661]=1==remove_fields[76663]=1==remove_fields[76664]=1==remove_fields[76666]=1==insertion===link_title_text= ==link_text= ==text_align===css_class=contentbrowser contentbrowser]"

  val sampleNode = NodeToConvert(List(contentTitle),
                                 Seq(),
                                 Some(CC_BY_SA.toString),
                                 Seq(author),
                                 List(tag),
                                 "fagstoff",
                                 "fagstoff",
                                 new Date(0),
                                 new Date(1),
                                 ArticleType.Standard,
                                 Seq.empty)

  val sampleMetaConvertedNode = NodeWithConvertedMeta(List(contentTitle),
                                                      Seq(),
                                                      CC_BY_SA.toString,
                                                      Seq(author),
                                                      List(tag),
                                                      "fagstoff",
                                                      "fagstoff",
                                                      new Date(0),
                                                      new Date(1),
                                                      Seq.empty,
                                                      ArticleType.Standard,
                                                      Seq.empty)

  val sampleLanguageContent =
    TestData.sampleContent.copy(content = sampleContentString, language = "nb")

  test("That the document is wrapped in an section tag") {
    val nodeId = "1"
    val initialContent = "<h1>Heading</h1>"
    val contentNode = sampleLanguageContent.copy(content = initialContent)
    val node = sampleNode.copy(contents = List(contentNode))
    val expedtedResult = s"<section>$initialContent</section>"

    when(extractConvertStoreContent.processNode("4321", ImportStatus.empty))
      .thenReturn(Try(TestData.sampleApiArticle, ImportStatus.empty))

    val Success((result: Article, status)) =
      service.toDomainArticle(node, ImportStatus.empty)

    result.content.head.content should equal(expedtedResult)
  }

  test("That content embedded in a node is converted") {
    val (nodeId, nodeId2) = ("1234", "4321")
    val altText =
      "Jente som spiser melom. Grønn bakgrunn, rød melon. Fotografi."
    val contentString =
      s"[contentbrowser ==nid=$nodeId==imagecache=Fullbredde==width===alt=$altText==link===node_link=1==link_type=link_to_content==lightbox_size===remove_fields[76661]=1==remove_fields[76663]=1==remove_fields[76664]=1==remove_fields[76666]=1==insertion=inline==link_title_text= ==link_text= ==text_align===css_class=contentbrowser contentbrowser]"
    val contentString2 =
      s"[contentbrowser ==nid=$nodeId2==imagecache=Fullbredde==width===alt=$altText==link===node_link=1==link_type=link_to_content==lightbox_size===remove_fields[76661]=1==remove_fields[76663]=1==remove_fields[76664]=1==remove_fields[76666]=1==insertion=inline==link_title_text= ==link_text= ==text_align===css_class=contentbrowser contentbrowser]"
    val sampleOppgave1 = NodeGeneralContent(nodeId, nodeId, "Tittel", s"Innhold! $contentString2", "nb")
    val sampleOppgave2 =
      NodeGeneralContent(nodeId, nodeId2, "Tittel", "Enda mer innhold!", "nb")
    val node = sampleNode.copy(contents = List(sampleLanguageContent.copy(content = contentString)))

    when(extractService.getNodeType(nodeId)).thenReturn(Some("oppgave"))
    when(extractService.getNodeGeneralContent(nodeId))
      .thenReturn(Seq(sampleOppgave1))
    when(extractService.getNodeData(nodeId)).thenReturn(Success(TestData.sampleNodeToConvert))

    when(extractService.getNodeType(nodeId2)).thenReturn(Some("oppgave"))
    when(extractService.getNodeGeneralContent(nodeId2))
      .thenReturn(Seq(sampleOppgave2))
    when(extractService.getNodeData(nodeId2)).thenReturn(Success(TestData.sampleNodeToConvert))

    val Success((result: Article, status)) =
      service.toDomainArticle(node, ImportStatus.empty)
    result.content.head.content should equal("<section>Innhold! Enda mer innhold!</section>")
    status.messages.isEmpty should equal(true)
    result.requiredLibraries.isEmpty should equal(true)
  }

  test("That the ingress is not added to the content") {
    val (nodeId, nodeId2) = ("1234", "4321")
    val ingressNodeBokmal = LanguageIngress("Hvem er sterkest?", None)
    val contentNodeBokmal =
      TestData.sampleContent.copy(content = "Nordavinden og sola kranglet en gang om hvem av dem som var den sterkeste",
                                  ingress = Some(ingressNodeBokmal))

    val ingressNodeNynorsk = LanguageIngress("Kven er sterkast?", None)
    val contentNodeNynorsk =
      TestData.sampleContent.copy(content = "Nordavinden og sola krangla ein gong om kven av dei som var den sterkaste",
                                  ingress = Some(ingressNodeNynorsk))

    val node =
      sampleNode.copy(contents = List(contentNodeBokmal, contentNodeNynorsk))
    val bokmalExpectedResult =
      "<section>Nordavinden og sola kranglet en gang om hvem av dem som var den sterkeste</section>"
    val nynorskExpectedResult =
      "<section>Nordavinden og sola krangla ein gong om kven av dei som var den sterkaste</section>"

    val Success((result: Article, status)) =
      service.toDomainArticle(node, ImportStatus.empty)
    val bokmalResult = result.content.head.content
    val nynorskResult = result.content.last.content

    bokmalResult should equal(bokmalExpectedResult)
    nynorskResult should equal(nynorskExpectedResult)
    status.messages.isEmpty should equal(true)
    result.requiredLibraries.isEmpty should equal(true)
  }

  test("ingress is extracted when wrapped in <p> tags") {
    val content =
      s"""<section>
        |<$ResourceHtmlEmbedTag data-size="fullbredde" data-align="" data-resource="image" data-alt="To personer" data-resource_id="5359" data-caption="capt.">
        |<p><strong>Når man driver med medieproduksjon, er det mye arbeid som må gjøres<br></strong></p>
        |</section>
        |<section> <p>Det som kan gi helse- og sikkerhetsproblemer på en dataarbeidsplass, er:</section>""".stripMargin
        .replace("\n", "")
    val expectedContentResult = ArticleContent(
      s"""<section>
         |<$ResourceHtmlEmbedTag data-size="fullbredde" data-align="" data-resource="image" data-alt="To personer" data-resource_id="5359" data-caption="capt.">
         |<p><strong>Når man driver med medieproduksjon, er det mye arbeid som må gjøres<br></strong></p>
         |</section>
         |<section><p>Det som kan gi helse- og sikkerhetsproblemer på en dataarbeidsplass, er:</p></section>""".stripMargin
        .replace("\n", ""),
      "nb"
    )

    val expectedIngressResult = ArticleIntroduction("Hvem er sterkest?", "nb")

    val ingressNodeBokmal = LanguageIngress("Hvem er sterkest?", None)
    val contentNodeBokmal =
      sampleLanguageContent.copy(content = content, ingress = Some(ingressNodeBokmal))

    val node = sampleNode.copy(contents = List(contentNodeBokmal))
    val Success((result: Article, status)) =
      service.toDomainArticle(node, ImportStatus.empty)

    result.content.length should be(1)
    result.introduction.length should be(1)
    result.content.head should equal(expectedContentResult)
    result.introduction.head should equal(expectedIngressResult)
  }

  test("That html attributes are removed from the article") {
    val contentNodeBokmal =
      sampleLanguageContent.copy(content = """<section><div class="testclass" title="test">high</div></section>""")
    val node = sampleNode.copy(contents = List(contentNodeBokmal))
    val bokmalExpectedResult = """<section><div>high</div></section>"""

    val Success((result: Article, status)) =
      service.toDomainArticle(node, ImportStatus.empty)

    result.content.head.content should equal(bokmalExpectedResult)
    status.messages.nonEmpty should equal(true)
    result.requiredLibraries.isEmpty should equal(true)
  }

  test("That align attributes for td tags are not removed") {
    val htmlTableWithAlignAttributes =
      """<section><table><tbody><tr><td align="right" valign="top">Table row cell</td></tr></tbody></table></section>"""
    val contentNodeBokmal =
      sampleLanguageContent.copy(content = htmlTableWithAlignAttributes)
    val node = sampleNode.copy(contents = List(contentNodeBokmal))
    val expectedResult =
      """<section><table><tbody><tr><td align="right" valign="top">Table row cell</td></tr></tbody></table></section>"""

    val Success((result: Article, status)) =
      service.toDomainArticle(node, ImportStatus.empty)

    result.content.head.content should equal(expectedResult)
    status.messages.isEmpty should equal(true)
    result.requiredLibraries.isEmpty should equal(true)
  }

  test("That html comments are removed") {
    val contentNodeBokmal = sampleLanguageContent.copy(
      content = """<section><p><!-- this is a comment -->not a comment</p><!-- another comment --></section>""")
    val node = sampleNode.copy(contents = List(contentNodeBokmal))
    val expectedResult = "<section><p>not a comment</p></section>"

    val Success((result: Article, status)) =
      service.toDomainArticle(node, ImportStatus.empty)

    result.content.head.content should equal(expectedResult)
    status.messages.isEmpty should equal(true)
    result.requiredLibraries.isEmpty should equal(true)
  }

  test("That images are converted") {
    val (nodeId, imageUrl, alt) = ("1234", "full.jpeg", "Fotografi")
    val newId = "1"
    val contentNode = sampleLanguageContent.copy(content = s"<section>$sampleContentString</section>")
    val node = sampleNode.copy(contents = List(contentNode))
    val imageMeta = ImageMetaInformation(newId,
                                         None,
                                         None,
                                         imageUrl,
                                         256,
                                         "",
                                         ImageCopyright(ImageLicense("", "", Some("")), "", List()),
                                         ImageTag(List(), ""))
    val expectedResult =
      s"""|<section>
          |<$ResourceHtmlEmbedTag data-align="" data-alt="$sampleAlt" data-caption="" data-resource="image" data-resource_id="1" data-size="full">
          |</section>""".stripMargin.replace("\n", "")

    when(extractService.getNodeType(nodeId)).thenReturn(Some("image"))
    when(imageApiClient.importImage(nodeId)).thenReturn(Some(imageMeta))
    val Success((result: Article, status)) =
      service.toDomainArticle(node, ImportStatus.empty)

    result.content.head.content should equal(expectedResult)
    result.requiredLibraries.length should equal(0)
  }

  test("&nbsp is removed") {
    val contentNodeBokmal = sampleLanguageContent.copy(content = """<section> <p>hello&nbsp; you</section>""")
    val node = sampleNode.copy(contents = List(contentNodeBokmal))
    val expectedResult = """<section><p>hello you</p></section>"""

    val Success((result: Article, status)) =
      service.toDomainArticle(node, ImportStatus.empty)
    val strippedResult =
      " +".r.replaceAllIn(result.content.head.content.replace("\n", ""), " ")

    strippedResult should equal(expectedResult)
    status.messages.isEmpty should equal(true)
    result.requiredLibraries.isEmpty should equal(true)
  }

  test("That empty html tags are removed") {
    val contentNodeBokmal =
      sampleLanguageContent.copy(content =
        s"""<section> <div></div><p><div></div></p><$ResourceHtmlEmbedTag data-resource="external" data-url="example.com"></$ResourceHtmlEmbedTag></section>""")
    val node = sampleNode.copy(contents = List(contentNodeBokmal))
    val expectedResult =
      s"""<section><$ResourceHtmlEmbedTag data-resource="external" data-url="example.com"></section>"""

    val Success((result: Article, status)) =
      service.toDomainArticle(node, ImportStatus.empty)

    result.content.head.content should equal(expectedResult)
    status.messages.isEmpty should equal(true)
    result.requiredLibraries.isEmpty should equal(true)
  }

  test("paragraphs are unwrapped if cell contains only one") {
    val table =
      s"""<table>
          |<tbody>
          |<tr>
          |<td><p>column</p></td>
          |</tr>
          |</tbody>
          |</table>""".stripMargin.replace("\n", "")

    val tableExpectedResult =
      s"""<table>
          |<tbody>
          |<tr>
          |<td>column</td>
          |</tr>
          |</tbody>
          |</table>""".stripMargin.replace("\n", "")

    val initialContent: LanguageContent =
      sampleLanguageContent.copy(content = table)
    val Success((content, _)) =
      TableConverter.convert(initialContent, ImportStatus.empty)
    content.content should equal(tableExpectedResult)
  }

  test("MathML elements are converted correctly") {
    val originalContent =
      "<section><math><menclose notation=\"updiagonalstrike\"></menclose>\u00a0</math></section>"
    val expectedContent =
      """<section><p><math xmlns="http://www.w3.org/1998/Math/MathML"><menclose notation="updiagonalstrike"></menclose> </math></p></section>"""
    val initialContent: LanguageContent =
      sampleLanguageContent.copy(content = originalContent)
    val node = sampleNode.copy(contents = List(initialContent))

    val Success((content: Article, _)) =
      service.toDomainArticle(node, ImportStatus.empty)

    content.content.head.content should equal(expectedContent)
  }

  test("toDomainArticle should return Success with an error if conversion fails") {
    val nodeId = 123123
    val contentString =
      s"[contentbrowser ==nid=$nodeId==imagecache=Fullbredde==width===alt=$sampleAlt==link===node_link=1==link_type=link_to_content==lightbox_size===remove_fields[76661]=1==remove_fields[76663]=1==remove_fields[76664]=1==remove_fields[76666]=1==insertion===link_title_text= ==link_text= ==text_align===css_class=contentbrowser contentbrowser]"
    val contentNodeBokmal = sampleLanguageContent.copy(content = contentString)
    val node = sampleNode.copy(contents = List(contentNodeBokmal))
    val expectedError =
      ImportException(nodeId.toString,
                      "ContentBrowserConverter failed",
                      Some(ImportException(s"$nodeId", s"Failed to import image with node id $nodeId")))
    val expectedResult =
      s"""<section><$ResourceHtmlEmbedTag data-message="Innhold mangler." data-resource="error"></section>"""

    when(extractService.getNodeType(s"$nodeId")).thenReturn(Some("image"))
    when(imageApiClient.importImage(s"$nodeId")).thenReturn(None)

    val Success((result: Article, status)) = service.toDomainArticle(node, ImportStatus.empty)

    status.errors.size should be(1)
    status.errors should equal(Seq(expectedError))
    result.content.map(_.content) should be(Seq(expectedResult))
  }

  test("toApiLicense defaults to unknown if the license was not found") {
    service.toApiLicense("invalid") should equal(api.License("unknown", None, None))
  }

  test("toApiLicense converts a short license string to a license object with description and url") {
    service.toApiLicense(CC_BY.toString) should equal(
      api.License("CC-BY-4.0",
                  Some("Creative Commons Attribution 4.0 International"),
                  Some("https://creativecommons.org/licenses/by/4.0/")))
  }

  test("VisualElement should be converted") {
    val node =
      sampleNode.copy(contents = List(TestData.sampleContent.copy(visualElement = Some(nodeId))))
    val expectedResult =
      s"""<$ResourceHtmlEmbedTag data-align="" data-alt="" data-caption="" data-resource="image" data-resource_id="1" data-size="" />"""
    when(extractService.getNodeType(nodeId)).thenReturn(Some("image"))
    when(imageApiClient.importImage(nodeId))
      .thenReturn(Some(TestData.sampleImageMetaInformation))

    val Success((convertedArticle: Article, _)) =
      service.toDomainArticle(node, ImportStatus.empty)
    convertedArticle.visualElement should equal(Seq(VisualElement(expectedResult, "en")))
  }

  test("That divs with class 'ndla_table' is converted to table") {
    val sampleLanguageContent: LanguageContent = TestData.sampleContent.copy(content =
      "<section><div class=\"ndla_table another_class\">nobody builds walls better than me, believe me</div></section>")
    val node = sampleNode.copy(contents = List(sampleLanguageContent))
    val expectedResult =
      "<section><table>nobody builds walls better than me, believe me</table></section>"
    val result = service.toDomainArticle(node, ImportStatus.empty)
    result.isSuccess should be(true)

    val Success((content: Article, _)) = result

    content.content.head.content should equal(expectedResult)
    content.requiredLibraries.length should equal(0)
  }

  test("Concepts should only contain plain text") {
    val sampleLanguageContent: LanguageContent =
      TestData.sampleContent.copy(content = "<h1>Nobody builds walls better than <strong>me, believe me</strong></h1>")
    val node = sampleNode.copy(contents = List(sampleLanguageContent), nodeType = "begrep")
    val expectedResult = "Nobody builds walls better than me, believe me"

    val Success((result: Concept, _)) =
      service.toDomainArticle(node, ImportStatus.empty)

    result.content.head.content should equal(expectedResult)
  }

  test("toDomainArticle should only include tags in relevant languages") {
    val titles = Seq(ArticleTitle("tiitel", "nb"))
    val contents = Seq(TestData.sampleContent.copy(language = "nb"))
    val tags =
      Seq(ArticleTag(Seq("t1", "t2"), "nb"), ArticleTag(Seq("t1", "t2"), "en"))

    val node =
      sampleMetaConvertedNode.copy(titles = titles, contents = contents, tags = tags)
    service.toDomainArticle(node).tags.map(_.language) should equal(Seq("nb"))
  }

  test("Leaf node converter should create an article from a pure h5p node") {
    when(h5pApiClient.getViewFromOldId("1234"))
      .thenReturn(Some(s"//ndla.no/h5p/embed/1234"))
    val sampleLanguageContent = TestData.sampleContent
      .copy(content = "<div><h1>hi</h1></div>", nodeType = "h5p_content")
    val expectedResult =
      s"""<section>${sampleLanguageContent.content}</section><section><$ResourceHtmlEmbedTag data-resource="external" data-url="//ndla.no/h5p/embed/1234"></section>"""
    val node = sampleNode.copy(contents = Seq(sampleLanguageContent), nodeType = "h5p_content", contentType = "oppgave")

    val Success((result: Article, _)) =
      service.toDomainArticle(node, ImportStatus.empty)

    result.content.head.content should equal(expectedResult)
    result.requiredLibraries.size should equal(0)
  }

  test("nbsp in MathML tags should be converted to space") {

    val sampleLanguageContent: LanguageContent =
      TestData.sampleContent.copy(content = "<section><p><math>\u00a0<mi>P\u00a0</mi></math></p></section>")
    val expectedResult =
      """<section><p><math xmlns="http://www.w3.org/1998/Math/MathML"> <mi>P </mi></math></p></section>"""
    val node = sampleNode.copy(contents = List(sampleLanguageContent))

    val result = service.toDomainArticle(node, ImportStatus.empty)
    result.isSuccess should be(true)

    val Success((content: Article, _)) = result

    content.content.head.content should equal(expectedResult)
  }

  test("nbsp in MathML <mo> tags should not be converted to space if only nbsp") {
    val sampleLanguageContent: LanguageContent =
      TestData.sampleContent.copy(content = "<section><p><math>\u00a0<mo>\u00a0</mo></math></p></section>")
    val expectedResult =
      "<section><p><math xmlns=\"http://www.w3.org/1998/Math/MathML\"> <mo>&#xa0;</mo></math></p></section>"
    val node = sampleNode.copy(contents = List(sampleLanguageContent))

    val result = service.toDomainArticle(node, ImportStatus.empty)
    result.isSuccess should be(true)

    val Success((content: Article, _)) = result

    content.content.head.content should equal(expectedResult)
  }

  test("That authors are translated correctly") {
    val authors = List(
      Author("Opphavsmann", "A"),
      Author("Redaksjonelt", "B"),
      Author("redaKsJoNelT", "C"),
      Author("distributør", "D"),
      Author("leVerandør", "E"),
      Author("Språklig", "F")
    )
    val copyright = service.toDomainCopyright(Some("by-sa"), authors)
    copyright.creators should contain(Author("Originator", "A"))

    copyright.rightsholders should contain(Author("Distributor", "D"))
    copyright.rightsholders should contain(Author("Supplier", "E"))

    copyright.processors should contain(Author("Linguistic", "F"))
    copyright.processors should contain(Author("Editorial", "B"))
    copyright.processors should contain(Author("Editorial", "C"))
  }

  test("That FilConverterModule + FileDivConverter converts contentbrowser to div in the correct position") {
    val title = "Full av trix"
    val filePath = s"$nodeId/test1.pdf"
    val filePath2 = s"$nodeId/test2.pdf"
    val fileMeta =
      ContentFilMeta(nodeId, "0", "title", "title.pdf", s"$Domain/files/title.pdf", "application/pdf", "1024")
    val fileMeta2 = fileMeta.copy(fileName = "title2.pdf", url = s"$Domain/files/title2.pdf")
    val expectedEmbed =
      s"""<div data-type="${ResourceType.File.toString}"><embed data-alt="Full av trix" data-path="files/$filePath" data-resource="${ResourceType.File.toString}" data-title="${fileMeta.title}" data-type="pdf"><embed data-alt="Full av trix" data-path="files/$filePath2" data-resource="${ResourceType.File.toString}" data-title="${fileMeta2.title}" data-type="pdf"></div>"""

    when(extractService.getNodeFilMeta(nodeId))
      .thenReturn(Success(Seq(fileMeta, fileMeta2)))
    when(attachmentStorageService.uploadFileFromUrl(nodeId, fileMeta))
      .thenReturn(Success(filePath))
    when(attachmentStorageService.uploadFileFromUrl(nodeId, fileMeta2))
      .thenReturn(Success(filePath2))
    when(extractService.getNodeType(nodeId)).thenReturn(Some("fil"))

    val contentBrowser =
      s"""[contentbrowser ==nid=$nodeId==remove_fields[76661]=1==remove_fields[76663]=1==remove_fields[76664]=1==remove_fields[76666]=1==width===insertion=link==link_title_text=$title==lightbox_size===link_text=$title==fid===text_align===css_class===alt===css_class=contentbrowser]"""

    val originalContent =
      s"""<section><p>Hei her er det vanlig tekst, men så plutselig: "$contentBrowser" whapam!</p><div><p><strong>Og en til $contentBrowser da</strong></p></div></section>"""
    val expectedResult =
      s"""<section><p>Hei her er det vanlig tekst, men så plutselig: "$title" whapam!</p>$expectedEmbed<div><p><strong>Og en til $title da</strong></p>$expectedEmbed</div></section>"""

    val sampleLanguageContent: LanguageContent = TestData.sampleContent.copy(content = originalContent)
    val node = sampleNode.copy(contents = List(sampleLanguageContent))

    val result = service.toDomainArticle(node, ImportStatus.empty)
    result.isSuccess should be(true)
    val Success((resultContent: Article, _)) = result

    resultContent.content.head.content should be(expectedResult)
  }

  test("That related articles are included in all languages") {
    val relatedContent = Seq(
      MigrationRelatedContent("1", "Heisann", "", 1),
      MigrationRelatedContent("2", "Hopsann", "", 1),
      MigrationRelatedContent("3", "Tralala", "", 1)
    )

    when(taxonomyApiClient.getResource(any[String])).thenReturn(Success(Some(TestData.sampleTaxonomyResource)))
    when(extractService.getNodeType(any[String])).thenReturn(Some("fagstoff"))
    when(extractService.getNodeData(any[String])).thenAnswer((i: InvocationOnMock) => {
      val nid = i.getArgument[String](0)
      Success(
        TestData.sampleNodeToConvert.copy(
          contents = Seq(
            TestData.sampleContent.copy(nid = nid, tnid = nid)
          )))
    })
    when(extractConvertStoreContent.processNode(any[String], any[ImportStatus])).thenAnswer((i: InvocationOnMock) => {
      val externalId = i.getArgument[String](0)
      val status = i.getArgument[ImportStatus](1)
      val importedId = ("1" + externalId).toInt
      Success((TestData.sampleApiArticle.copy(id = importedId), status))
    })

    val nbLanguageContent = TestData.sampleContent.copy(language = "nb",
                                                        content = "<section><p>Hei hå</p></section>",
                                                        relatedContent = relatedContent)
    val enLanguageContent = TestData.sampleContent.copy(language = "en",
                                                        content = "<section><p>Hey ho</p></section>",
                                                        relatedContent = relatedContent)

    val relatedSection =
      """<section>
        |<div data-type="related-content">
        |<embed data-article-id="11" data-resource="related-content">
        |<embed data-article-id="12" data-resource="related-content">
        |<embed data-article-id="13" data-resource="related-content">
        |</div>
        |</section>""".stripMargin.replace("\n", "")
    val expectedNbContent = s"<section><p>Hei hå</p></section>$relatedSection"
    val expectedEnContent = s"<section><p>Hey ho</p></section>$relatedSection"

    val node = sampleNode.copy(contents = List(nbLanguageContent, enLanguageContent))
    val Success((result: Article, _)) = service.toDomainArticle(node, ImportStatus.empty.withNewNodeLocalContext())
    result.content.map(_.content) should be(Seq(expectedNbContent, expectedEnContent))
  }

  test("That single file embeds are moved to after inline, but text is left inline") {

    val fileNodeId = "6666"
    val taxonomyResource = Resource("urn:resource:1:123", "fint navn", None, "/fin/path/egentlig")
    when(taxonomyApiClient.getResource(any[String])).thenReturn(Success(Some(taxonomyResource)))
    when(extractService.getNodeType(any[String])).thenReturn(Some("fagstoff"))
    when(extractService.getNodeType(fileNodeId)).thenReturn(Some("fil"))
    when(extractService.getNodeData(any[String])).thenAnswer((i: InvocationOnMock) => {
      val nid = i.getArgument[String](0)
      Success(
        TestData.sampleNodeToConvert.copy(
          contents = Seq(
            TestData.sampleContent.copy(nid = nid, tnid = nid)
          )))
    })

    reset(extractConvertStoreContent)
    when(extractConvertStoreContent.processNode(any[String], any[ImportStatus])).thenAnswer((i: InvocationOnMock) => {
      val externalId = i.getArgument[String](0)
      val status = i.getArgument[ImportStatus](1)
      val importedId = ("1" + externalId).toInt
      Success((TestData.sampleApiArticle.copy(id = importedId), status))
    })
    when(extractService.getNodeFilMeta(fileNodeId)).thenReturn(
      Success(
        Seq(
          ContentFilMeta(
            fileNodeId,
            fileNodeId,
            "Woop woop all the files",
            "all_the_files.pdf",
            "http://ndla.no/sites/default/files/all_the_filerino.pdf",
            "application/pdf",
            "78547"
          )
        )))
    when(attachmentStorageService.uploadFileFromUrl(eqTo(fileNodeId), any[ContentFilMeta])).thenReturn(
      Success("yabadaba/all_the_filerino.pdf")
    )

    val fileContentBrowser =
      s"[contentbrowser ==nid=6666==imagecache=Fullbredde==width===alt===link===node_link=1==link_type=link_to_content==lightbox_size===remove_fields[76661]=1==remove_fields[76663]=1==remove_fields[76664]=1==remove_fields[76666]=1==insertion===link_title_text= ==link_text=Her er den fine linken==text_align===css_class=contentbrowser contentbrowser]"

    val content =
      s"""<section><p>Hei hå, her har vi inlinefiler $fileContentBrowser, jaddajadda!</p></section>""".stripMargin

    val languageContent = TestData.sampleContent.copy(language = "nb", content = content)

    val expectedContent =
      s"""<section><p>Hei hå, her har vi inlinefiler Her er den fine linken, jaddajadda!</p><div data-type="file"><embed data-alt="Her er den fine linken" data-path="files/yabadaba/all_the_filerino.pdf" data-resource="file" data-title="Woop woop all the files" data-type="pdf"></div></section>""".stripMargin

    val node = sampleNode.copy(contents = List(languageContent))
    val Success((result: Article, _)) = service.toDomainArticle(node, ImportStatus.empty.withNewNodeLocalContext())
    result.content.map(_.content) should be(Seq(expectedContent))

  }

}
