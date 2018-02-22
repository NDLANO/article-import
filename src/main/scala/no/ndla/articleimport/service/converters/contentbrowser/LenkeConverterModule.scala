/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters.contentbrowser

import com.netaporter.uri.Uri.parse
import com.netaporter.uri.dsl._
import com.typesafe.scalalogging.LazyLogging
import no.ndla.articleimport.model.api.ImportException
import no.ndla.articleimport.model.domain.{ImportStatus, RequiredLibrary}
import no.ndla.articleimport.service.ExtractService
import no.ndla.articleimport.service.converters.HtmlTagGenerator
import no.ndla.validation.ResourceType
import org.jsoup.Jsoup
import no.ndla.articleimport.integration.ConverterModule.stringToJsoupDocument
import no.ndla.articleimport.integration.MigrationEmbedMeta

import scala.util.{Failure, Success, Try}

trait LenkeConverterModule {
  this: ExtractService with HtmlTagGenerator =>

  object LenkeConverter extends ContentBrowserConverterModule with LazyLogging {
    override val typeName: String = "lenke"

    override def convert(content: ContentBrowser, importStatus: ImportStatus): Try[(String, Seq[RequiredLibrary], ImportStatus)] = {
      logger.info(s"Converting lenke with nid ${content.get("nid")}")

      convertLink(content) match {
        case Success((linkHtml, requiredLibraries, errors)) => Success(linkHtml, requiredLibraries, importStatus.addMessages(errors))
        case Failure(x) => Failure(x)
      }
    }

    def convertLink(cont: ContentBrowser): Try[(String, Seq[RequiredLibrary], Seq[String])] = {
      val LightboxPattern = "(lightbox_.*)".r

      val embedMeta = extractService.getNodeEmbedMeta(cont.get("nid"))
        .map(meta => meta.copy(url = meta.url.orElse(tryFetchSrcAttributeFromTag(meta.embedCode.getOrElse("")))))

      embedMeta match {
        case Success(MigrationEmbedMeta(Some(url), embedCode)) =>
          val (htmlTag, requiredLibrary, errors) = cont.get("insertion") match {
            case "inline" => insertInline(url, embedCode.getOrElse(""))
            case "link" | "collapsed_body" | LightboxPattern(_) => insertAnchor(url, cont)
            case _ => insertUnhandled(url, cont)
          }

          val NDLAPattern = """.*(ndla.no).*""".r
          val warnings =  Try(parse(url)) match {
            case Success(uri) => uri.host.getOrElse("") match {
              case NDLAPattern(_) => Seq(s"Link to NDLA old resource: '$url'")
              case _ => Seq()
            }
            case Failure(_) => Seq(s"Link in article is invalid: '$url'")
          }

          warnings.foreach(msg => logger.warn(msg))
          Success((htmlTag, requiredLibrary.toList, errors ++ warnings))
        case Success(MigrationEmbedMeta(url, embedCode)) => Failure(ImportException(s"External embed meta is missing url or embed code (url='$url', embedCode='$embedCode')"))
        case Failure(_) => Failure(ImportException(s"Failed to import embed metadata for node id ${cont.get("nid")}"))
      }
    }

    private def tryFetchSrcAttributeFromTag(tag: String): Option[String] = {
      Option(stringToJsoupDocument(tag).select("[src]").attr("src"))
        .filter(_.trim.nonEmpty)
    }

    private def insertInline(url: String, embedCode: String): (String, Option[RequiredLibrary], Seq[String]) = {
      val message = s"External resource to be embedded: $url"
      logger.info(message)

      val NRKUrlPattern = """(.*\.?nrk.no)""".r
      val PreziUrlPattern = """(.*\.?prezi.com)""".r
      val CommonCraftUrlPattern = """(.*\.?commoncraft.com)""".r
      val NdlaFilmIundervisningUrlPattern = """(.*\.?ndla.filmiundervisning.no)""".r
      val KahootUrlPattern = """(.*\.?play.kahoot.it)""".r
      val vimeoProUrlPattern = """(.*\.?vimeopro.com)""".r
      val khanAcademyUrlPattern = """(.*\.?khanacademy.org)""".r
      val tv2SkoleUrlPattern = """(.*\.?tv2skole.no)""".r
      val vgNoUrlPattern = """(.*\.?vg.no)""".r
      val scribdUrlPattern = """(.*\.?scribd.com)""".r
      val kunnskapsFilmUrlPattern = """(.*\.?kunnskapsfilm.no)""".r

      val (embedTag, requiredLibs) = url.host.getOrElse("") match {
        case NRKUrlPattern(_) => getNrkEmbedTag(embedCode, url)
        case vimeoProUrlPattern(_) => getVimeoProEmbedTag(embedCode)
        case kunnskapsFilmUrlPattern(_) => getKunnskapsFilmEmbedTag(embedCode)
        case PreziUrlPattern(_) |
             CommonCraftUrlPattern(_) |
             NdlaFilmIundervisningUrlPattern(_) |
             KahootUrlPattern(_) |
             khanAcademyUrlPattern(_) |
             tv2SkoleUrlPattern(_) |
             scribdUrlPattern(_) |
             vgNoUrlPattern(_) =>
          getRegularEmbedTag(embedCode)
        case _ => (HtmlTagGenerator.buildExternalInlineEmbedContent(url), None)
      }
      (embedTag, requiredLibs, message :: Nil)
    }

    def getNrkEmbedTag(embedCode: String, url: String): (String, Option[RequiredLibrary]) = {
      val doc = Jsoup.parseBodyFragment(embedCode)
      val (videoId, requiredLibraryUrl) = (doc.select("div[data-nrk-id]").attr("data-nrk-id"), doc.select("script").attr("src"))
      val requiredLibrary = RequiredLibrary("text/javascript", "NRK video embed", requiredLibraryUrl.copy(scheme=None))

      (HtmlTagGenerator.buildNRKInlineVideoContent(videoId, url), Some(requiredLibrary))
    }

    def getRegularEmbedTag(embedCode: String): (String, Option[RequiredLibrary]) = {
      val doc = Jsoup.parseBodyFragment(embedCode).select("iframe").first()
      val (src, width, height) = (doc.attr("src"), doc.attr("width"), doc.attr("height"))

      (HtmlTagGenerator.buildRegularInlineContent(src, width, height, ResourceType.IframeContent), None)
    }

    def getVimeoProEmbedTag(embedCode: String): (String, Option[RequiredLibrary]) = {
      val doc = Jsoup.parseBodyFragment(embedCode).select("iframe").first()
      val src = doc.attr("src")

      (HtmlTagGenerator.buildExternalInlineEmbedContent(src), None)
    }

    def getKunnskapsFilmEmbedTag(embedCode: String): (String, Option[RequiredLibrary]) = {
      val doc = Jsoup.parseBodyFragment(embedCode).select("iframe").first()
      val src = doc.attr("src")

      // Since all sources seem to be vimeo urls, we simply use data-resource=external to let oembed-proxy handle these
      (HtmlTagGenerator.buildExternalInlineEmbedContent(src), None)
    }

    private def insertDetailSummary(url: String, embedCode: String, cont: ContentBrowser): (String, Option[RequiredLibrary], Seq[String]) = {
      val (elementToInsert, requiredLib, figureErrors) = insertInline(url, embedCode)
      (s"<details><summary>${cont.get("link_text")}</summary>$elementToInsert</details>", requiredLib, figureErrors)
    }

    private def insertAnchor(url: String, cont: ContentBrowser): (String, Option[RequiredLibrary], Seq[String]) = {
      val urlWithFragment = cont.getOpt("link_anchor").map(anchor => url.withFragment(anchor).toString)

      val htmlTag = HtmlTagGenerator.buildAnchor(urlWithFragment.getOrElse(url), cont.get("link_text"), cont.get("link_title_text"), true)
      (s" $htmlTag", None, Seq())
    }

    private def insertUnhandled(url: String, cont: ContentBrowser): (String, Option[RequiredLibrary], Seq[String]) = {
      val (anchor, requiredLib, anchorErrors) = insertAnchor(url, cont)
      val message = s"""Unhandled insertion method '${cont.get("insertion")}' on '${cont.get("link_text")}'. Defaulting to link."""

      logger.warn(message)
      (anchor, requiredLib, anchorErrors :+ message)
    }
  }

}
