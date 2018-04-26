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
import no.ndla.articleimport.integration.MigrationEmbedMeta
import no.ndla.articleimport.service.DomainRegex._

import scala.util.{Failure, Success, Try}

trait LenkeConverterModule {
  this: ExtractService with HtmlTagGenerator =>

  object LenkeConverter extends ContentBrowserConverterModule with LazyLogging {
    override val typeName: String = "lenke"

    override def convert(content: ContentBrowser,
                         importStatus: ImportStatus): Try[(String, Seq[RequiredLibrary], ImportStatus)] = {
      logger.info(s"Converting lenke with nid ${content.get("nid")}")

      convertLink(content) match {
        case Success((linkHtml, requiredLibraries, errors)) =>
          Success(linkHtml, requiredLibraries, importStatus.addMessages(errors))
        case Failure(x) => Failure(x)
      }
    }

    def convertLink(cont: ContentBrowser): Try[(String, Seq[RequiredLibrary], Seq[String])] = {
      val LightboxPattern = "(lightbox_.*)".r
      val externalId = cont.get("nid")

      extractService.getLinkEmbedMeta(externalId) match {
        case Success(MigrationEmbedMeta(Some(url), embedCode)) =>
          val inserted = cont.get("insertion") match {
            case "inline"                                       => insertInline(externalId, url, embedCode.getOrElse(""))
            case "link" | "collapsed_body" | LightboxPattern(_) => insertAnchor(url, cont)
            case _                                              => insertUnhandled(url, cont)
          }

          val NDLAPattern = "ndla.no".asDomainRegex
          val warnings = Try(parse(url)) match {
            case Success(uri) =>
              uri.host.getOrElse("") match {
                case NDLAPattern(_) => Seq(s"Link to NDLA old resource: '$url'")
                case _              => Seq()
              }
            case Failure(_) => Seq(s"Link in article is invalid: '$url'")
          }
          warnings.foreach(msg => logger.warn(msg))

          inserted.map {
            case (htmlTag, requiredLibrary, errors) =>
              (htmlTag, requiredLibrary.toList, errors ++ warnings)
          }
        case Success(MigrationEmbedMeta(url, embedCode)) =>
          Failure(
            ImportException(externalId,
                            s"External embed meta is missing url or embed code (url='$url', embedCode='$embedCode')"))
        case Failure(_) =>
          Failure(ImportException(externalId, s"Failed to import embed metadata for node id ${cont.get("nid")}"))
      }
    }

    private val embedHostWhitelist = List(
      "commoncraft.com",
      "exprogroup.com",
      "kahoot.it",
      "khanacademy.org",
      "ndla.filmiundervisning.no",
      "nrk.no",
      "prezi.com",
      "scribd.com",
      "slideshare.net",
      "ted.com",
      "tv2skole.no",
      "vg.no",
      "vimeo.com",
      "youtu.be",
      "youtube.com"
    ).map(_.asDomainRegex)

    private def urlIsWhitelisted(url: String): Boolean = {
      val host = url.host.getOrElse("")
      embedHostWhitelist.exists(_.pattern.matcher(host).matches())
    }

    def insertInline(nid: String,
                     url: String,
                     embedCode: String): Try[(String, Option[RequiredLibrary], Seq[String])] = {
      val message = s"External resource to be embedded: $url"
      logger.info(message)

      if (urlIsWhitelisted(url)) {
        val NRKUrlPattern = "nrk.no".asDomainRegex
        val PreziUrlPattern = "prezi.com".asDomainRegex
        val CommonCraftUrlPattern = "commoncraft.com".asDomainRegex
        val NdlaFilmIundervisningUrlPattern = "ndla.filmiundervisning.no".asDomainRegex
        val KahootUrlPattern = "play.kahoot.it".asDomainRegex
        val vimeoProUrlPattern = "vimeopro.com".asDomainRegex
        val khanAcademyUrlPattern = "khanacademy.org".asDomainRegex
        val tv2SkoleUrlPattern = "tv2skole.no".asDomainRegex
        val vgNoUrlPattern = "vg.no".asDomainRegex
        val scribdUrlPattern = "scribd.com".asDomainRegex
        val kunnskapsFilmUrlPattern = "kunnskapsfilm.no".asDomainRegex

        val (embedTag, requiredLibs) = url.host.getOrElse("") match {
          case NRKUrlPattern(_)           => getNrkEmbedTag(embedCode, url)
          case vimeoProUrlPattern(_)      => getVimeoProEmbedTag(embedCode)
          case kunnskapsFilmUrlPattern(_) => getKunnskapsFilmEmbedTag(embedCode)
          case PreziUrlPattern(_) | CommonCraftUrlPattern(_) | NdlaFilmIundervisningUrlPattern(_) |
              KahootUrlPattern(_) | khanAcademyUrlPattern(_) | tv2SkoleUrlPattern(_) | scribdUrlPattern(_) |
              vgNoUrlPattern(_) =>
            getRegularEmbedTag(embedCode)
          case _ => (HtmlTagGenerator.buildExternalInlineEmbedContent(url), None)
        }
        Success((embedTag, requiredLibs, message :: Nil))
      } else {
        Failure(ImportException(nid, s"'$url'is not a whitelisted embed source"))
      }
    }

    def getNrkEmbedTag(embedCode: String, url: String): (String, Option[RequiredLibrary]) = {
      val doc = Jsoup.parseBodyFragment(embedCode)
      val (videoId, requiredLibraryUrl) =
        (doc.select("div[data-nrk-id]").attr("data-nrk-id"), doc.select("script").attr("src"))
      val requiredLibrary =
        RequiredLibrary("text/javascript", "NRK video embed", requiredLibraryUrl.copy(scheme = None))

      (HtmlTagGenerator.buildNRKInlineVideoContent(videoId, url), Some(requiredLibrary))
    }

    def getRegularEmbedTag(embedCode: String): (String, Option[RequiredLibrary]) = {
      val doc = Jsoup.parseBodyFragment(embedCode).select("iframe").first()
      val (src, width, height) =
        (doc.attr("src"), doc.attr("width"), doc.attr("height"))

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

    private def insertDetailSummary(nid: String,
                                    url: String,
                                    embedCode: String,
                                    cont: ContentBrowser): Try[(String, Option[RequiredLibrary], Seq[String])] = {
      insertInline(nid, url, embedCode) match {
        case Success((elementToInsert, requiredLib, figureErrors)) =>
          Success(
            (s"<details><summary>${cont.get("link_text")}</summary>$elementToInsert</details>",
             requiredLib,
             figureErrors))
        case Failure(ex) => Failure(ex)
      }
    }

    private def insertAnchor(url: String, cont: ContentBrowser): Try[(String, Option[RequiredLibrary], Seq[String])] = {
      val urlWithFragment = cont.getOpt("link_anchor").map(anchor => url.withFragment(anchor).toString)

      val htmlTag = HtmlTagGenerator.buildAnchor(urlWithFragment.getOrElse(url),
                                                 cont.get("link_text"),
                                                 cont.get("link_title_text"),
                                                 true)
      Success(s" $htmlTag", None, Seq())
    }

    private def insertUnhandled(url: String,
                                cont: ContentBrowser): Try[(String, Option[RequiredLibrary], Seq[String])] = {
      insertAnchor(url, cont) match {
        case Success((anchor, requiredLib, anchorErrors)) =>
          val message =
            s"""Unhandled insertion method '${cont.get("insertion")}' on '${cont.get("link_text")}'. Defaulting to link."""

          logger.warn(message)
          Success(anchor, requiredLib, anchorErrors :+ message)
        case Failure(ex) => Failure(ex)
      }
    }
  }

}
