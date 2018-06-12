/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters.contentbrowser

import no.ndla.articleimport.{TestData, TestEnvironment, UnitSuite}
import no.ndla.articleimport.integration.MigrationEmbedMeta
import no.ndla.articleimport.model.api.ImportException
import no.ndla.articleimport.model.domain.ImportStatus
import no.ndla.validation.ResourceType
import no.ndla.validation.EmbedTagRules.ResourceHtmlEmbedTag
import org.mockito.Mockito._

import scala.util.{Failure, Success}

class LenkeConverterModuleTest extends UnitSuite with TestEnvironment {
  val nodeId = "1234"
  val altText = "Jente som spiser melom. Grønn bakgrunn, rød melon. Fotografi."
  val linkUrl = "https://www.youtube.com/watch?v=1qN72LEQnaU"

  override val extractService = new ExtractService

  override def beforeEach = {
    val iframeEmbed = s"""<iframe src="$linkUrl" />"""
    when(extractService.getNodeEmbedMeta(nodeId))
      .thenReturn(Success(MigrationEmbedMeta(Some(linkUrl), Some(iframeEmbed))))
  }

  test("That LenkeConverter returns an embed code if insertion method is 'inline'") {
    val content =
      TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "alt" -> altText, "insertion" -> "inline")

    val Success((result, requiredLibraries, status)) =
      LenkeConverterModule.convert(content, ImportStatus.empty)
    result should equal(s"""<$ResourceHtmlEmbedTag data-resource="external" data-url="$linkUrl" />""")
    requiredLibraries.length should equal(0)
    status.messages should be(Seq("External resource to be embedded: https://www.youtube.com/watch?v=1qN72LEQnaU"))
  }

  test("That LenkeConverter returns an a-tag if insertion method is 'link'") {
    val content =
      TestData.contentBrowserWithFields(List.empty,
                                        "nid" -> nodeId,
                                        "alt" -> altText,
                                        "link_text" -> " ",
                                        "insertion" -> "link")
    val expectedResult =
      """ <a href="https://www.youtube.com/watch?v=1qN72LEQnaU" rel="noopener noreferrer" target="_blank" title=""> </a>"""

    val Success((result, requiredLibraries, errors)) =
      LenkeConverterModule.convert(content, ImportStatus.empty)
    result should equal(expectedResult)
    requiredLibraries.length should equal(0)
    errors.messages.length should equal(0)
  }

  test("That LenkeConverter defaults to 'link' if insertion method is not handled") {
    val content = TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "alt" -> altText, "link_text" -> " ")
    val expectedResult =
      """ <a href="https://www.youtube.com/watch?v=1qN72LEQnaU" rel="noopener noreferrer" target="_blank" title=""> </a>"""

    val Success((result, requiredLibraries, status)) =
      LenkeConverterModule.convert(content, ImportStatus.empty)
    result should equal(expectedResult)
    requiredLibraries.length should equal(0)
    status.messages should be(Seq("Unhandled insertion method '' on ' '. Defaulting to link."))
  }

  test("That LenkeConverter returns an a-tag if insertion method is 'lightbox_large'") {
    val content =
      TestData.contentBrowserWithFields(List.empty,
                                        "nid" -> nodeId,
                                        "alt" -> altText,
                                        "link_text" -> " ",
                                        "insertion" -> "lightbox_large")
    val expectedResult =
      """ <a href="https://www.youtube.com/watch?v=1qN72LEQnaU" rel="noopener noreferrer" target="_blank" title=""> </a>"""

    val Success((result, requiredLibraries, errors)) =
      LenkeConverterModule.convert(content, ImportStatus.empty)
    result should equal(expectedResult)
    requiredLibraries.length should equal(0)
    errors.messages.length should equal(0)
  }

  test("That LenkeConverter returns an a-tag if insertion method is 'collapsed_body'") {
    val content =
      TestData.contentBrowserWithFields(List.empty,
                                        "nid" -> nodeId,
                                        "alt" -> altText,
                                        "link_text" -> " ",
                                        "insertion" -> "collapsed_body")
    val expectedResult =
      s""" <a href="https://www.youtube.com/watch?v=1qN72LEQnaU" rel="noopener noreferrer" target="_blank" title=""> </a>"""

    val Success((result, requiredLibraries, errors)) =
      LenkeConverterModule.convert(content, ImportStatus.empty)

    result should equal(expectedResult)
    requiredLibraries.length should equal(0)
    errors.messages.length should equal(0)
  }

  test("That LenkeConverter returns inline content with nrk video id") {
    val nrkVideoId = "94605"
    val nrkScriptUrl = "https://www.nrk.no/serum/latest/js/video_embed.js"
    val nrkEmbedScript =
      s"""<div class="nrk-video" data-nrk-id="$nrkVideoId"></div><script src="$nrkScriptUrl"></script>"""
    val nrkLinkUrl =
      "http://nrk.no/skole/klippdetalj?topic=urn%3Ax-mediadb%3A18745"
    val content =
      TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "alt" -> altText, "insertion" -> "inline")
    val expectedResult =
      s"""<$ResourceHtmlEmbedTag data-nrk-video-id="$nrkVideoId" data-resource="nrk" data-url="$nrkLinkUrl" />"""

    when(extractService.getNodeEmbedMeta(nodeId))
      .thenReturn(Success(MigrationEmbedMeta(Some(nrkLinkUrl), Some(nrkEmbedScript))))
    val Success((result, requiredLibraries, status)) =
      LenkeConverterModule.convert(content, ImportStatus.empty)

    result should equal(expectedResult)
    status.messages should be(Seq("External resource to be embedded: http://nrk.no/skole/klippdetalj?topic=urn%3Ax-mediadb%3A18745"))
    requiredLibraries.length should equal(1)
    requiredLibraries.head.url should equal(nrkScriptUrl.replace("https:", ""))
  }

  test("That LenkeConverter returns a iframe embed for prezi resources") {
    val preziUrl = "http://prezi.com/123123123"
    val preziSrc = "https://prezi.com/embed/123123123&autoplay=0"
    val preziEmbedCode =
      s"""<iframe id="iframe_container" frameborder="0" webkitallowfullscreen="" mozallowfullscreen="" allowfullscreen="" width="620" height="451" src="$preziSrc"></iframe>"""

    val content =
      TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "alt" -> altText, "insertion" -> "inline")
    val expectedResult =
      s"""<$ResourceHtmlEmbedTag data-height="451" data-resource="${ResourceType.IframeContent}" data-url="$preziSrc" data-width="620" />"""

    when(extractService.getNodeEmbedMeta(nodeId))
      .thenReturn(Success(MigrationEmbedMeta(Some(preziUrl), Some(preziEmbedCode))))
    val Success((result, _, status)) =
      LenkeConverterModule.convert(content, ImportStatus.empty)

    result should equal(expectedResult)
    status.messages should be(Seq("External resource to be embedded: http://prezi.com/123123123"))
  }

  test("That LenkeConverter returns a iframe embed for commoncraft resources") {
    val CcraftUrl = "http://www.commoncraft.com/123123123"
    val CcraftSrc = "https://www.commoncraft.com/embed/db233ba&autoplay=0"
    val CcraftEmbedCode =
      s"""<iframe id="cc-embed" frameborder="0" width="620" height="451" src="$CcraftSrc" scrolling="false"></iframe>"""

    val content =
      TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "alt" -> altText, "insertion" -> "inline")
    val expectedResult =
      s"""<$ResourceHtmlEmbedTag data-height="451" data-resource="${ResourceType.IframeContent}" data-url="$CcraftSrc" data-width="620" />"""

    when(extractService.getNodeEmbedMeta(nodeId))
      .thenReturn(Success(MigrationEmbedMeta(Some(CcraftUrl), Some(CcraftEmbedCode))))
    val Success((result, _, status)) =
      LenkeConverterModule.convert(content, ImportStatus.empty)

    result should equal(expectedResult)
    status.messages should be(Seq("External resource to be embedded: http://www.commoncraft.com/123123123"))
  }

  test("That LenkeConverter returns a iframe embed for ndla.filmundervisningen resources") {
    val NdlaFilmUrl =
      "https://ndla.filmiundervisning.no/film/ndlafilm.aspx?filmId=12412"
    val NdlaFilmSrc =
      "//ndla.filmiundervisning.no/film/ndlafilm.aspx?filmId=12412"
    val NdlaFilmEmbedCode =
      s"""<iframe src="$NdlaFilmSrc" style="border: none;" frameBorder="0" width="632px" height="337px" allowfullscreen></iframe>"""

    val content =
      TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "alt" -> altText, "insertion" -> "inline")
    val expectedResult =
      s"""<$ResourceHtmlEmbedTag data-height="337px" data-resource="${ResourceType.IframeContent}" data-url="$NdlaFilmSrc" data-width="632px" />"""

    when(extractService.getNodeEmbedMeta(nodeId))
      .thenReturn(Success(MigrationEmbedMeta(Some(NdlaFilmUrl), Some(NdlaFilmEmbedCode))))
    val Success((result, _, status)) =
      LenkeConverterModule.convert(content, ImportStatus.empty)

    result should equal(expectedResult)
    status.messages should be(Seq("External resource to be embedded: https://ndla.filmiundervisning.no/film/ndlafilm.aspx?filmId=12412"))
  }

  test("That LenkeConverter returns a iframe embed for kahoot resources") {
    val KahootUrl =
      "https://play.kahoot.it/#/k/e577f7e9-59ff-4a80-89a1-c95acf04815d"
    val KahootSrc =
      "https://embed.kahoot.it/e577f7e9-59ff-4a80-89a1-c95acf04815d"
    val KahootEmbedCode =
      s"""<iframe src="$KahootSrc" name="iframe1" scrolling="no" frameborder="no" align="center" height = "350px" width = "620px"></iframe>"""

    val content =
      TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "alt" -> altText, "insertion" -> "inline")
    val expectedResult =
      s"""<$ResourceHtmlEmbedTag data-height="350px" data-resource="${ResourceType.IframeContent}" data-url="$KahootSrc" data-width="620px" />"""

    when(extractService.getNodeEmbedMeta(nodeId))
      .thenReturn(Success(MigrationEmbedMeta(Some(KahootUrl), Some(KahootEmbedCode))))
    val Success((result, _, status)) =
      LenkeConverterModule.convert(content, ImportStatus.empty)

    result should equal(expectedResult)
    status.messages should be(Seq("External resource to be embedded: https://play.kahoot.it/#/k/e577f7e9-59ff-4a80-89a1-c95acf04815d"))
  }

  test("LenkeConverter should include an url fragment if defined in contentbrowser") {
    val anchor = "9a-4"
    val content =
      TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "insertion" -> "link", "link_anchor" -> anchor)

    val Success((result, _, errors)) =
      LenkeConverterModule.convert(content, ImportStatus.empty)

    result should equal(s""" <a href="$linkUrl#$anchor" rel="noopener noreferrer" target="_blank" title=""></a>""")
    errors.messages.length should equal(0)
  }

  test("LenkeConverter should return an error if embedmeta does not contain embedcode or url") {
    val content = TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId)

    when(extractService.getNodeEmbedMeta(nodeId))
      .thenReturn(Success(MigrationEmbedMeta(None, None)))
    val Failure(x: ImportException) =
      LenkeConverterModule.convert(content, ImportStatus.empty)
    x.message.contains("External embed meta is missing url or embed code") should be(true)
  }

  test(
    "LenkeConverter should return an error if embedmeta does not contain url and a src attr cant be found in embed code") {
    val content = TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId)

    when(extractService.getNodeEmbedMeta(nodeId))
      .thenReturn(Success(MigrationEmbedMeta(None, Some("<h1>this makes no sense</h1>"))))
    val Failure(x: ImportException) =
      LenkeConverterModule.convert(content, ImportStatus.empty)
    x.message.contains("External embed meta is missing url or embed code") should be(true)
  }

  test("LenkeConverter should use url from embedCode if url is undefined") {
    val content = TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "insertion" -> "inline")

    when(extractService.getNodeEmbedMeta(nodeId))
      .thenReturn(Success(MigrationEmbedMeta(None, Some("<script src='http://nrk.no'></script>"))))
    val Success((result, _, errors)) = LenkeConverterModule.convert(content, ImportStatus.empty)

    result should equal(
      s"""<$ResourceHtmlEmbedTag data-nrk-video-id="" data-resource="${ResourceType.NRKContent}" data-url="http://nrk.no" />""")
  }

  test("only whitelisted hosts should be embedded") {
    val content = TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "insertion" -> "inline")
    when(extractService.getNodeEmbedMeta(nodeId)).thenReturn(
      Success(MigrationEmbedMeta(Some("http://obscure.stuff.gg"), Some("<script src='http://hmmm.biz.niz"))))
    val Failure(ex: ImportException) = LenkeConverterModule.convert(content, ImportStatus.empty)

    ex.message.contains("not a whitelisted embed source") should be(true)
  }
}
