/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters

import no.ndla.articleimport.integration.LanguageContent
import no.ndla.articleimport.model.api.ImportExceptions
import no.ndla.articleimport.model.domain.ImportStatus
import no.ndla.articleimport.{TestData, TestEnvironment, UnitSuite}

import scala.util.{Failure, Success}

class SimpleTagConverterTest extends UnitSuite with TestEnvironment {
  val nodeId = "1234"

  test("That divs with class 'paragraph' are replaced with section") {
    val sampleLanguageContent = TestData.sampleContent.copy(
      content = "<h1>heading</h1><div class='paragraph'>I know words, I have the best words.</div>")
    val expectedResult =
      "<h1>heading</h1><section>I know words, I have the best words.</section>"
    val Success((result, _)) =
      SimpleTagConverter.convert(sampleLanguageContent, ImportStatus.empty)

    result.content should equal(expectedResult)
    result.requiredLibraries.size should equal(0)
  }

  test("That divs with class 'full' are removed") {
    val initialContent = TestData.sampleContent.copy(
      content = "<article><div class='full'><h1>heading</h1>A small loan of a million dollars</div></article>")
    val expectedResult =
      "<article><h1>heading</h1>A small loan of a million dollars</article>"
    val Success((result, _)) =
      SimpleTagConverter.convert(initialContent, ImportStatus.empty)

    result.content should equal(expectedResult)
    result.requiredLibraries.size should equal(0)
  }

  test("That children of pre tags are wrapped in code tags") {
    val initialContent =
      TestData.sampleContent.copy(content = "<h1>heading</h1><pre>I know words, I have the best words.</pre>")
    val expectedResult =
      "<h1>heading</h1><pre><code>I know words, I have the best words.</code></pre>"
    val Success((result, _)) =
      SimpleTagConverter.convert(initialContent, ImportStatus.empty)

    result.content should equal(expectedResult)
    result.requiredLibraries.size should equal(0)
  }

  test("That divs with class 'quote' are replaced with a blockquote tag") {
    val initialContent = TestData.sampleContent.copy(
      content = """<article><h1>heading</h1><div class="quote">I know words, I have the best words.</div></article>""")
    val expectedResult =
      "<article><h1>heading</h1><blockquote>I know words, I have the best words.</blockquote></article>"
    val Success((result, _)) =
      SimpleTagConverter.convert(initialContent, ImportStatus.empty)

    result.content should equal(expectedResult)
    result.requiredLibraries.size should equal(0)
  }

  test("That divs with class 'right' are replaced with a aside tag") {
    val initialContent = TestData.sampleContent.copy(
      content = """<article><div class="right">I know words, I have the best words.</div></article>""")
    val expectedResult =
      "<article><aside>I know words, I have the best words.</aside></article>"
    val Success((result, _)) =
      SimpleTagConverter.convert(initialContent, ImportStatus.empty)

    result.content should equal(expectedResult)
    result.requiredLibraries.size should equal(0)
  }

  test("That divs with class 'hide' converted to details-summary tags") {
    val initialContent =
      TestData.sampleContent.copy(content = """<div class="hide">Eksempel: <a href="#" class="read-more">les mer</a>
          |<div class="details">
            |<p>Hello, this is content</p>
            |<a class="re-collapse" href="#">skjul</a>
          |</div>
        |</div>""".stripMargin.replace("\n", ""))
    val expectedResult =
      "<details><summary>Eksempel: les mer</summary><p>Hello, this is content</p></details>"
    val Success((result, _)) =
      SimpleTagConverter.convert(initialContent, ImportStatus.empty)

    result.content should equal(expectedResult)
    result.requiredLibraries.size should equal(0)
  }

  test("That divs with class 'frame' convertet to class c-bodybox") {
    val initialContent = TestData.sampleContent.copy(content =
      """<div class="frame"><h4>De fire friheter</h4><p>Fri bevegelse av</p><ul><li>varer</li><li>tjenester</li><li>kapital </li><li>personer</li></ul></div>""")
    val expectedResult =
      """<div class="c-bodybox"><h4>De fire friheter</h4><p>Fri bevegelse av</p><ul><li>varer</li><li>tjenester</li><li>kapital </li><li>personer</li></ul></div>"""
    val Success((result, _)) =
      SimpleTagConverter.convert(initialContent, ImportStatus.empty)

    result.content should equal(expectedResult)
  }

  test("That body is converted to article") {
    val initialContent = TestData.sampleContent.copy(
      content = """<body><div class="right">I know words, I have the best words.</div></body>""")
    val expectedResult = "<aside>I know words, I have the best words.</aside>"
    val Success((result, _)) =
      SimpleTagConverter.convert(initialContent, ImportStatus.empty)

    result.content should equal(expectedResult)
    result.requiredLibraries.size should equal(0)
  }

  test("That divs with class 'ndla_table' is converted to table") {
    val sampleLanguageContent: LanguageContent = TestData.sampleContent.copy(content =
      "<article><div class=\"ndla_table another_class\">nobody builds walls better than me, believe me</div></article>")
    val expectedResult =
      "<article><table>nobody builds walls better than me, believe me</table></article>"
    val result =
      SimpleTagConverter.convert(sampleLanguageContent, ImportStatus.empty)
    result.isSuccess should be(true)

    val Success((content, _)) = result

    content.content should equal(expectedResult)
    content.requiredLibraries.size should equal(0)
  }

  test("That divs with class 'ndla_table_row' is converted to tr") {
    val initialContent: LanguageContent = TestData.sampleContent.copy(content =
      "<article><div class=\"ndla_table_row another_class\">My IQ is one of the highest - and you all know it!</div></article>")
    val expectedResult =
      "<article><tr>My IQ is one of the highest - and you all know it!</tr></article>"
    val result = SimpleTagConverter.convert(initialContent, ImportStatus.empty)
    result.isSuccess should be(true)

    val Success((content, _)) = result
    content.content should equal(expectedResult)
    content.requiredLibraries.size should equal(0)
  }

  test("That divs with class 'ndla_table_cell' is converted to td") {
    val initialContent: LanguageContent = TestData.sampleContent.copy(
      content = "<article><div class=\"ndla_table_cell another_class\">I am very highly educated</div></article>")
    val expectedResult = "<article><td>I am very highly educated</td></article>"
    val result = SimpleTagConverter.convert(initialContent, ImportStatus.empty)
    result.isSuccess should be(true)

    val Success((content, _)) = result
    content.content should equal(expectedResult)
    content.requiredLibraries.size should equal(0)
  }

  test("That divs with class 'ndla_table_cell_content' is removed") {
    val initialContent: LanguageContent = TestData.sampleContent.copy(content =
      "<article><div><div class=\"ndla_table_cell_content another_class\">in that wall we are going to have a big fat door</div></div></article>")
    val expectedResult =
      "<article><div>in that wall we are going to have a big fat door</div></article>"
    val result = SimpleTagConverter.convert(initialContent, ImportStatus.empty)
    result.isSuccess should be(true)

    val Success((content, _)) = result
    content.content should equal(expectedResult)
    content.requiredLibraries.size should equal(0)
  }

  test("That h3s with class 'frame' convertet to div and class c-bodybox") {
    val initialContent = TestData.sampleContent.copy(content =
      """<h3 class="frame"><p>De fire friheter</p><p>Fri bevegelse av</p><ul><li>varer</li><li>tjenester</li><li>kapital </li><li>personer</li></ul></h2>""")
    val expectedResult =
      """<div class="c-bodybox"><p>De fire friheter</p><p>Fri bevegelse av</p><ul><li>varer</li><li>tjenester</li><li>kapital </li><li>personer</li></ul></div>"""
    val Success((result, _)) =
      SimpleTagConverter.convert(initialContent, ImportStatus.empty)

    result.content should equal(expectedResult)
  }

  test("That h2s with class 'frame' convertet to div and class c-bodybox") {
    val initialContent = TestData.sampleContent.copy(content =
      """<h2 class="frame"><p>De fire friheter</p><p>Fri bevegelse av</p><ul><li>varer</li><li>tjenester</li><li>kapital </li><li>personer</li></ul></h2>""")
    val expectedResult =
      """<div class="c-bodybox"><p>De fire friheter</p><p>Fri bevegelse av</p><ul><li>varer</li><li>tjenester</li><li>kapital </li><li>personer</li></ul></div>"""
    val Success((result, _)) =
      SimpleTagConverter.convert(initialContent, ImportStatus.empty)

    result.content should equal(expectedResult)
  }

  test("That h2s with no class is untouched") {
    val initialContent =
      TestData.sampleContent.copy(content = """<h2>Test</h2>""")
    val expectedResult = """<h2>Test</h2>"""
    val Success((result, _)) =
      SimpleTagConverter.convert(initialContent, ImportStatus.empty)

    result.content should equal(expectedResult)
  }

  test("only spans with font-size attribute around chinese text should be replaced with a data-size attribute") {
    val initialContent = TestData.sampleContent.copy(content = """<span style="font-size: xx-large;">第一课：汉字</span>""")
    val expectedResult = """<span data-size="large">第一课：汉字</span>"""
    val Success((result, _)) =
      SimpleTagConverter.convert(initialContent, ImportStatus.empty)

    result.content should equal(expectedResult)

    val contentWithMultipleStylingElements = TestData.sampleContent.copy(
      content = """<span style="font-size   :xx-large   ; another-attribute: hmm">第一课：汉字</span>""")
    val expectedResult2 = """<span data-size="large">第一课：汉字</span>"""
    val Success((result2, _)) =
      SimpleTagConverter.convert(contentWithMultipleStylingElements, ImportStatus.empty)

    result2.content should equal(expectedResult2)

    val contentWithoutChinese =
      TestData.sampleContent.copy(content = """<span style="font-size: xx-large;">hello</span>""")
    val expectedResult3 = """<span style="font-size: xx-large;">hello</span>"""
    val Success((result3, _)) =
      SimpleTagConverter.convert(contentWithoutChinese, ImportStatus.empty)

    result3.content should equal(expectedResult3)
  }

  test("spans with xml:lang attribute is kept as <span> tags and lang tag is inserted") {
    val content =
      s"""<section>
         |<span xml:lang="nb">HyperText Markup Language</span>
         |</section>""".stripMargin.replace("\n", "")
    val expectedContentResult =
      s"""<section>
         |<span lang="nb">HyperText Markup Language</span>
         |</section>""".stripMargin.replace("\n", "")

    val Success((result, _)) =
      SimpleTagConverter.convert(TestData.sampleContent.copy(content = content), ImportStatus.empty)

    result.content should equal(expectedContentResult)
  }

  test("That embed-tags from old ndla makes converter insert error-embed and succeed") {
    val content =
      """
        |<section>
        |<div>
        |<p>Test</p>
        |<embed type="application/x-shockwave-flash" src="http://example.com/test.swf" />
        |</div>
        |</section>
      """.stripMargin

    val expectedContent =
      """
        |<section>
        |<div>
        |<p>Test</p>
        |<embed data-message="Innhold mangler." data-resource="error">
        |</div>
        |</section>
      """.stripMargin

    val Success((result, status)) =
      SimpleTagConverter.convert(TestData.sampleContent.copy(content = content), ImportStatus.empty)

    result.content should be(expectedContent)
    status.errors should be(Seq(
      "Failed to import node with invalid embed, src was: 'http://example.com/test.swf', and type was: 'application/x-shockwave-flash'."))
  }

}
