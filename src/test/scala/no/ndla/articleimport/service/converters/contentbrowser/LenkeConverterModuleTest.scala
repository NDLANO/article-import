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
import no.ndla.articleimport.integration.ConverterModule.stringToJsoupDocument
import no.ndla.validation.ResourceType
import no.ndla.validation.EmbedTagRules.ResourceHtmlEmbedTag
import org.mockito.Mockito._
import no.ndla.articleimport.caching.Memoize
import io.lemonlabs.uri.dsl._
import no.ndla.network.model.HttpRequestException
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito
import scalaj.http.{Http, HttpRequest, HttpResponse}

import scala.util.{Failure, Success, Try}

class LenkeConverterModuleTest extends UnitSuite with TestEnvironment {
  val nodeId = "1234"
  val altText = "Jente som spiser melom. Grønn bakgrunn, rød melon. Fotografi."
  val linkUrl = "https://www.youtube.com/watch?v=1qN72LEQnaU"

  override val extractService: ExtractService = new ExtractService

  def memoizeMockGetNodeEmbedData(ret: Try[MigrationEmbedMeta]): Memoize[String, Try[MigrationEmbedMeta]] = {
    Memoize[String, Try[MigrationEmbedMeta]]((_: String) => ret)
  }

  override def beforeEach: Unit = {
    val iframeEmbed = s"""<iframe src="$linkUrl" />"""

    when(migrationApiClient.getNodeEmbedData).thenReturn({
      memoizeMockGetNodeEmbedData(Success(MigrationEmbedMeta(Some(linkUrl), Some(iframeEmbed))))
    })
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

    when(migrationApiClient.getNodeEmbedData)
      .thenReturn(memoizeMockGetNodeEmbedData(Success(MigrationEmbedMeta(Some(nrkLinkUrl), Some(nrkEmbedScript)))))
    val Success((result, requiredLibraries, status)) =
      LenkeConverterModule.convert(content, ImportStatus.empty)

    result should equal(expectedResult)
    status.messages should be(
      Seq("External resource to be embedded: http://nrk.no/skole/klippdetalj?topic=urn%3Ax-mediadb%3A18745"))
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

    when(migrationApiClient.getNodeEmbedData)
      .thenReturn(memoizeMockGetNodeEmbedData(Success(MigrationEmbedMeta(Some(preziUrl), Some(preziEmbedCode)))))
    val Success((result, _, status)) =
      LenkeConverterModule.convert(content, ImportStatus.empty)

    result should equal(expectedResult)
    status.messages should be(Seq("External resource to be embedded: http://prezi.com/123123123"))
  }

  test("That LenkeConverter returns an external embed for vimeopro resources") {
    val vimeoProUrl = "http://www.vimeopro.com/heiho/film/video/12345"
    val vimeoProSrc = "https://player.vimeo.com/video/12345"
    val vimeoProEmbedCode =
      s"""<iframe src=\"$vimeoProSrc\" width=\"620\" height=\"501\" frameborder=\"0\" webkitallowfullscreen mozallowfullscreen allowfullscreen></iframe>"""

    val content =
      TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "alt" -> altText, "insertion" -> "inline")
    val expectedResult =
      s"""<$ResourceHtmlEmbedTag data-resource="${ResourceType.ExternalContent}" data-url="$vimeoProSrc" />"""

    when(migrationApiClient.getNodeEmbedData)
      .thenReturn(memoizeMockGetNodeEmbedData(Success(MigrationEmbedMeta(Some(vimeoProUrl), Some(vimeoProEmbedCode)))))
    val Success((result, _, status)) =
      LenkeConverterModule.convert(content, ImportStatus.empty)

    result should equal(expectedResult)
    status.messages should be(Seq(s"External resource to be embedded: $vimeoProUrl"))
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
      s"""<$ResourceHtmlEmbedTag data-height="337px" data-resource="${ResourceType.IframeContent}" data-url="${NdlaFilmSrc
        .withScheme("https")}" data-width="632px" />"""

    when(migrationApiClient.getNodeEmbedData)
      .thenReturn(memoizeMockGetNodeEmbedData(Success(MigrationEmbedMeta(Some(NdlaFilmUrl), Some(NdlaFilmEmbedCode)))))
    val Success((result, _, status)) =
      LenkeConverterModule.convert(content, ImportStatus.empty)

    result should equal(expectedResult)
    status.messages should be(
      Seq("External resource to be embedded: https://ndla.filmiundervisning.no/film/ndlafilm.aspx?filmId=12412"))
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

    when(migrationApiClient.getNodeEmbedData)
      .thenReturn(memoizeMockGetNodeEmbedData(Success(MigrationEmbedMeta(Some(KahootUrl), Some(KahootEmbedCode)))))
    val Success((result, _, status)) =
      LenkeConverterModule.convert(content, ImportStatus.empty)

    result should equal(expectedResult)
    status.messages should be(
      Seq("External resource to be embedded: https://play.kahoot.it/#/k/e577f7e9-59ff-4a80-89a1-c95acf04815d"))
  }

  test("That LenkeConverter returns an external embed for geogebra resources") {
    val geogebraUrl =
      "https://www.geogebra.org/material/iframe/id/juVBCefc/width/958/height/547/border/888888/smb/false/stb/false/stbh/false/ai/false/asb/false/sri/false/rc/false/ld/false/sdz/false/ctl/false"
    val geogebraEmbedCode =
      s"""<iframe scrolling="no" title="Finn sannsynligheten for at tegnestiften lander med spissen ned!" src="$geogebraUrl" width="632px" height="361px" style="border:0px;"> </iframe>"""

    val content =
      TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "alt" -> altText, "insertion" -> "inline")
    val expectedResult =
      s"""<$ResourceHtmlEmbedTag data-height="361px" data-resource="${ResourceType.IframeContent}" data-url="$geogebraUrl" data-width="632px" />"""

    when(migrationApiClient.getNodeEmbedData)
      .thenReturn(memoizeMockGetNodeEmbedData(Success(MigrationEmbedMeta(Some(geogebraUrl), Some(geogebraEmbedCode)))))
    val Success((result, _, status)) =
      LenkeConverterModule.convert(content, ImportStatus.empty)

    result should equal(expectedResult)
    status.messages should be(Seq(s"External resource to be embedded: $geogebraUrl"))
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

    when(migrationApiClient.getNodeEmbedData)
      .thenReturn(memoizeMockGetNodeEmbedData(Success(MigrationEmbedMeta(None, None))))
    val Failure(x: ImportException) =
      LenkeConverterModule.convert(content, ImportStatus.empty)
    x.message.contains("External embed meta is missing url or embed code") should be(true)
  }

  test(
    "LenkeConverter should return an error if embedmeta does not contain url and a src attr cant be found in embed code") {
    val content = TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId)

    when(migrationApiClient.getNodeEmbedData)
      .thenReturn(memoizeMockGetNodeEmbedData(Success(MigrationEmbedMeta(None, Some("<h1>this makes no sense</h1>")))))
    val Failure(x: ImportException) =
      LenkeConverterModule.convert(content, ImportStatus.empty)
    x.message.contains("External embed meta is missing url or embed code") should be(true)
  }

  test("LenkeConverter should use url from embedCode if url is undefined") {
    val content = TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "insertion" -> "inline")

    when(migrationApiClient.getNodeEmbedData)
      .thenReturn(
        memoizeMockGetNodeEmbedData(Success(MigrationEmbedMeta(None, Some("<script src='http://nrk.no'></script>")))))
    val Success((result, _, _)) = LenkeConverterModule.convert(content, ImportStatus.empty)

    result should equal(
      s"""<$ResourceHtmlEmbedTag data-nrk-video-id="" data-resource="${ResourceType.NRKContent}" data-url="http://nrk.no" />""")
  }

  test("only whitelisted hosts should be embedded") {
    val content = TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "insertion" -> "inline")
    when(migrationApiClient.getNodeEmbedData).thenReturn(
      memoizeMockGetNodeEmbedData(
        Success(MigrationEmbedMeta(Some("http://obscure.stuff.gg"), Some("<script src='http://hmmm.biz.niz")))))
    val Failure(ex: ImportException) = LenkeConverterModule.convert(content, ImportStatus.empty)

    ex.message.contains("not a whitelisted embed source") should be(true)
  }

  test("domains should be converted to https and add a warning if unreachable") {
    val src = "new.livestream.com/video/123"
    val errorResponse =
      new HttpRequestException(s"Received error 404 NOT FOUND when calling https://$src. Body was ''", None)

    val content = TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "insertion" -> "inline")
    when(migrationApiClient.getNodeEmbedData).thenReturn(
      memoizeMockGetNodeEmbedData(
        Success(MigrationEmbedMeta(Some("http://livestream.com/video/123"), Some(s"<iframe src='http://$src'>")))))
    val lenkeConverterModule = spy(LenkeConverterModule)
    when(lenkeConverterModule.checkAvailability(any[String])).thenReturn(Left(Some(errorResponse)))
    val Success((result, _, status)) = lenkeConverterModule.convert(content, ImportStatus.empty)
    result should be(s"""<embed data-height="" data-resource="iframe" data-url="https://$src" data-width="" />""")
    status.errors should be(
      Seq(ImportException(
        "1234",
        "Embed with 'http://livestream.com/video/123' might not be rendered properly, as 'https://new.livestream.com/video/123' returned an error.",
        Some(errorResponse)
      )))
  }

  test("Youtube urls should keep original queryParams, but get time params from embedCodeUrl") {
    val embedCode =
      "<iframe width=\"640\" height=\"360\" src=\"https://www.youtube.com/embed/eW5XdFqeTW4?rel=0&showinfo=0&start=5&end=167\" frameborder=\"0\" allowfullscreen></iframe>"
    val url = "https://youtu.be/eW5XdFqeTW4?start=123&lel=lel"

    val newEmbed = LenkeConverterModule.buildYoutubeEmbedTag(embedCode, url)
    val newUrl = stringToJsoupDocument(newEmbed).select("embed").first().attr("data-url")

    newUrl.query.params.sortBy(_._1) should be(
      Vector("start" -> Some("5"), "end" -> Some("167"), "lel" -> Some("lel"), "rel" -> Some("0")).sortBy(_._1))
  }

  test("youtube and youtu.be executes the same function and yields correct results") {
    val lenkeConverterModule = spy(LenkeConverterModule)
    val content = TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "insertion" -> "inline")
    val embedCode =
      "<iframe width=\"640\" height=\"360\" src=\"https://www.youtube.com/embed/eW5XdFqeTW4?showinfo=0&start=5&end=167\" frameborder=\"0\" allowfullscreen></iframe>"

    val url = "https://youtu.be/eW5XdFqeTW4?start=123&lel=lel"
    val url2 = "https://youtube.com/watch?v=eW5XdFqeTW4&start=123&lel=lel"

    when(migrationApiClient.getNodeEmbedData)
      .thenReturn(memoizeMockGetNodeEmbedData(Success(MigrationEmbedMeta(Some(url), Some(embedCode)))))
      .thenReturn(memoizeMockGetNodeEmbedData(Success(MigrationEmbedMeta(Some(url2), Some(embedCode)))))

    val Success((result, _, _)) = lenkeConverterModule.convert(content, ImportStatus.empty)
    result should be(
      s"""<$ResourceHtmlEmbedTag data-resource="external" data-url="https://youtu.be/eW5XdFqeTW4?lel=lel&start=5&end=167" />""")
    verify(lenkeConverterModule, times(1)).buildYoutubeEmbedTag(embedCode, url)

    val Success((result2, _, _)) = lenkeConverterModule.convert(content, ImportStatus.empty)
    result2 should be(
      s"""<$ResourceHtmlEmbedTag data-resource="external" data-url="https://youtube.com/watch?v=eW5XdFqeTW4&lel=lel&start=5&end=167" />""")
    verify(lenkeConverterModule, times(1)).buildYoutubeEmbedTag(embedCode, url2)
  }

  test("youtube and youtu.be urls should keep rel=0 query parameter") {
    val lenkeConverterModule = spy(LenkeConverterModule)
    val content = TestData.contentBrowserWithFields(List.empty, "nid" -> nodeId, "insertion" -> "inline")
    val embedCode =
      "<iframe width=\"640\" height=\"360\" src=\"https://www.youtube.com/embed/eW5XdFqeTW4?rel=0&showinfo=0&start=5&rel=0&denna=nopls&end=167\" frameborder=\"0\" allowfullscreen></iframe>"

    val url = "https://youtu.be/eW5XdFqeTW4?start=123"

    when(migrationApiClient.getNodeEmbedData)
      .thenReturn(memoizeMockGetNodeEmbedData(Success(MigrationEmbedMeta(Some(url), Some(embedCode)))))

    val Success((result, _, _)) = lenkeConverterModule.convert(content, ImportStatus.empty)
    result should be(
      s"""<$ResourceHtmlEmbedTag data-resource="external" data-url="https://youtu.be/eW5XdFqeTW4?start=5&rel=0&end=167" />""")
    verify(lenkeConverterModule, times(1)).buildYoutubeEmbedTag(embedCode, url)
  }

  test("Youtube embeds on another format should be handled correctly") {
    val embedCode =
      "<object style=\"height: 390px; width: 640px\"><param name=\"movie\" value=\"http://www.youtube.com/v/ezgUPIgX8z8?version=3\"><param name=\"allowFullScreen\" value=\"true\"><param name=\"allowScriptAccess\" value=\"always\"><embed src=\"http://www.youtube.com/v/ezgUPIgX8z8?version=3\" type=\"application/x-shockwave-flash\" allowfullscreen=\"true\" allowScriptAccess=\"always\" width=\"640\" height=\"390\"></object>"
    val url = "https://youtu.be/ezgUPIgX8z8"

    val newEmbed = LenkeConverterModule.buildYoutubeEmbedTag(embedCode, url)
    val newUrl = stringToJsoupDocument(newEmbed).select("embed").first().attr("data-url")

    newUrl should be(url)
    newUrl.query.params.sortBy(_._1) should be(Vector())
  }

  test("That youtube embeds without embed codes just uses url") {
    val url = "https://youtu.be/ezgUPIgX8z8"

    val newEmbed = LenkeConverterModule.buildYoutubeEmbedTag("", url)
    val newUrl = stringToJsoupDocument(newEmbed).select("embed").first().attr("data-url")

    newUrl should be(url)
  }
}
