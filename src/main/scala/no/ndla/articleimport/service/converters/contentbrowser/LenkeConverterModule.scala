/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service.converters.contentbrowser

import io.lemonlabs.uri.dsl._
import io.lemonlabs.uri.Url
import com.typesafe.scalalogging.LazyLogging
import no.ndla.articleimport.model.api.ImportException
import no.ndla.articleimport.model.domain.{ImportStatus, RequiredLibrary}
import no.ndla.articleimport.service.ExtractService
import no.ndla.articleimport.service.converters.HtmlTagGenerator
import no.ndla.validation.ResourceType
import org.jsoup.Jsoup
import no.ndla.articleimport.integration.MigrationEmbedMeta
import no.ndla.articleimport.service.DomainRegex._
import no.ndla.articleimport.integration.ConverterModule.stringToJsoupDocument
import no.ndla.network.NdlaClient
import scalaj.http.{Http, HttpRequest}

import scala.util.{Failure, Success, Try}

trait LenkeConverterModule {
  this: ExtractService with NdlaClient with HtmlTagGenerator =>

  object LenkeConverterModule extends ContentBrowserConverterModule with LazyLogging {
    override val typeName: String = "lenke"

    override def convert(content: ContentBrowser,
                         importStatus: ImportStatus): Try[(String, Seq[RequiredLibrary], ImportStatus)] = {
      logger.info(s"Converting lenke with nid ${content.get("nid")}")

      convertLink(content, importStatus) match {
        case Success((linkHtml, requiredLibraries, status)) =>
          Success((linkHtml, requiredLibraries, status))
        case Failure(x) => Failure(x)
      }
    }

    def convertLink(cont: ContentBrowser,
                    importStatus: ImportStatus): Try[(String, Seq[RequiredLibrary], ImportStatus)] = {
      val LightboxPattern = "(lightbox_.*)".r
      val externalId = cont.get("nid")

      extractService.getLinkEmbedMeta(externalId) match {
        case Success(MigrationEmbedMeta(Some(url), embedCode)) =>
          val inserted = cont.get("insertion") match {
            case "inline" =>
              insertInline(externalId, url, embedCode.getOrElse(""), importStatus)
            case "link" | "collapsed_body" | LightboxPattern(_) => insertAnchor(url, cont, importStatus)
            case _                                              => insertUnhandled(url, cont, importStatus)
          }

          val NDLAPattern = "ndla.no".asDomainRegex
          val warnings = Try(Url.parse(url)) match {
            case Success(uri) =>
              val host = uri.hostOption.map(_.toString())
              host.getOrElse("") match {
                case NDLAPattern(_) => Seq(s"Link to NDLA old resource: '$url'")
                case _              => Seq.empty
              }
            case Failure(_) => Seq(s"Link in article is invalid: '$url'")
          }
          warnings.foreach(msg => logger.warn(msg))

          inserted.map {
            case (htmlTag, requiredLibrary, status) =>
              (htmlTag, requiredLibrary.toList, status.addMessages(warnings))
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
      "vimeopro.com",
      "youtu.be",
      "youtube.com",
      "issuu.com",
      "livestream.com",
      "channel9.msdn.com",
      "tomknudsen.no"
    ).map(_.asDomainRegex)

    private def urlIsWhitelisted(url: String): Boolean = {
      val host = Url.parse(url).hostOption.map(_.toString).getOrElse("")
      embedHostWhitelist.exists(_.pattern.matcher(host).matches())
    }

    def insertInline(nid: String,
                     url: String,
                     embedCode: String,
                     importStatus: ImportStatus): Try[(String, Option[RequiredLibrary], ImportStatus)] = {
      val message = s"External resource to be embedded: $url"
      logger.info(message)

      if (urlIsWhitelisted(url)) {
        val updatedStatus = importStatus.addMessage(message)

        val NRKUrlPattern = "nrk.no".asDomainRegex
        val PreziUrlPattern = "prezi.com".asDomainRegex
        val NdlaFilmIundervisningUrlPattern = "ndla.filmiundervisning.no".asDomainRegex
        val KahootUrlPattern = "play.kahoot.it".asDomainRegex
        val vimeoProUrlPattern = "vimeopro.com".asDomainRegex
        val khanAcademyUrlPattern = "khanacademy.org".asDomainRegex
        val tv2SkoleUrlPattern = "tv2skole.no".asDomainRegex
        val vgNoUrlPattern = "vg.no".asDomainRegex
        val scribdUrlPattern = "scribd.com".asDomainRegex
        val kunnskapsFilmUrlPattern = "kunnskapsfilm.no".asDomainRegex
        val livestreamUrlPattern = "livestream.com".asDomainRegex
        val channel9MsdnUrlPattern = "channel9.msdn.com".asDomainRegex
        val tomknudsenUrlPattern = "tomknudsen.no".asDomainRegex
        val youtubecomUrlPattern = "youtube.com".asDomainRegex
        val youtubeUrlPattern = "youtu.be".asDomainRegex

        url.hostOption.map(_.toString).getOrElse("") match {
          case NRKUrlPattern(_) =>
            val (embed, requiredLib) = getNrkEmbedTag(embedCode, url); Success((embed, requiredLib, updatedStatus))
          case vimeoProUrlPattern(_)      => Success(getVimeoProEmbedTag(embedCode), None, updatedStatus)
          case kunnskapsFilmUrlPattern(_) => Success(getKunnskapsFilmEmbedTag(embedCode), None, updatedStatus)
          case PreziUrlPattern(_) | NdlaFilmIundervisningUrlPattern(_) | KahootUrlPattern(_) |
              khanAcademyUrlPattern(_) | tv2SkoleUrlPattern(_) | scribdUrlPattern(_) | vgNoUrlPattern(_) |
              livestreamUrlPattern(_) | channel9MsdnUrlPattern(_) | tomknudsenUrlPattern(_) =>
            buildRegularEmbedTag(embedCode, nid, url, updatedStatus).map {
              case (embedTag, status) => (embedTag, None, status)
            }
          case youtubecomUrlPattern(_) | youtubeUrlPattern(_) =>
            Success(buildYoutubeEmbedTag(embedCode, url), None, updatedStatus)
          case _ => Success((HtmlTagGenerator.buildExternalInlineEmbedContent(url), None, updatedStatus))
        }
      } else {
        Failure(ImportException(nid, s"'$url' is not a whitelisted embed source"))
      }
    }

    /**
      * Builds embedTag for youtube embeds.
      *
      * Special for youtube is that some query parameters are transferred from the embedcode source to the url.
      * This is because start- and stop-time queryparameters is just added for the embedcode source and not the url.
      *
      * @param embedCode iframe that embedded youtube in old system
      * @param url Url to be used in the resulting embed
      * @return Embed-tag html
      */
    private[service] def buildYoutubeEmbedTag(embedCode: String, url: String): String = {
      val paramTypesToTransfer = List("t", "time_continue", "start", "end", "rel")
      val doc = stringToJsoupDocument(embedCode).select("iframe").first()
      val embedUrl = doc.attr("src")
      val queryParamsToTransfer = embedUrl.query.filterNames(pn => paramTypesToTransfer.contains(pn))

      val newUrl = queryParamsToTransfer.params.foldLeft(url) { (url, parameter) =>
        url.replaceParams(parameter._1, parameter._2).toString
      }

      HtmlTagGenerator.buildExternalInlineEmbedContent(newUrl)
    }

    def getNrkEmbedTag(embedCode: String, url: String): (String, Option[RequiredLibrary]) = {
      val doc = Jsoup.parseBodyFragment(embedCode)
      val (videoId, requiredLibraryUrl) =
        (doc.select("div[data-nrk-id]").attr("data-nrk-id"), doc.select("script").attr("src"))
      val requiredLibrary =
        RequiredLibrary("text/javascript", "NRK video embed", requiredLibraryUrl.withScheme("").toString.drop(1)) // drop(1) so we get //example.com rather than ://example.com

      (HtmlTagGenerator.buildNRKInlineVideoContent(videoId, url), Some(requiredLibrary))
    }

    private def buildRegularEmbedTag(embedCode: String,
                                     nid: String,
                                     url: String,
                                     importStatus: ImportStatus): Try[(String, ImportStatus)] = {

      Option(stringToJsoupDocument(embedCode).select("iframe").first()).map(doc => {
        val (src, width, height) =
          (doc.attr("src"), doc.attr("width"), doc.attr("height"))
        val urlWithHttps = src.withScheme("https")

        val status = checkAvailability(urlWithHttps) match {
          case Left(error) =>
            importStatus.addError(
              ImportException(
                nid,
                s"Embed with '$url' might not be rendered properly, as '$urlWithHttps' returned an error.",
                error))
          case Right(_) => importStatus
        }

        (HtmlTagGenerator.buildRegularInlineContent(urlWithHttps, width, height, ResourceType.IframeContent), status)
      }) match {
        case None                     => Failure(ImportException(nid, s"embed code for '$url' is undefined"))
        case Some((embedTag, status)) => Success(embedTag, status)
      }
    }

    /**
      * @param url Url of which to test availability
      * @return Either which is Left (With Option of exception if thrown) if request failed somehow, and right if successful.
      */
    private[contentbrowser] def checkAvailability(url: String): Either[Option[Throwable], Int] = {
      Try(Http(url).asString) match {
        case Success(s) if s.is2xx => Right(s.code)
        case Success(_)            => Left(None)
        case Failure(ex)           => Left(Some(ex))
      }
    }

    private def getVimeoProEmbedTag(embedCode: String): String = {
      val doc = Jsoup.parseBodyFragment(embedCode).select("iframe").first()
      val src = doc.attr("src")

      HtmlTagGenerator.buildExternalInlineEmbedContent(src)
    }

    private def getKunnskapsFilmEmbedTag(embedCode: String): String = {
      val doc = Jsoup.parseBodyFragment(embedCode).select("iframe").first()
      val src = doc.attr("src")

      // Since all sources seem to be vimeo urls, we simply use data-resource=external to let oembed-proxy handle these
      HtmlTagGenerator.buildExternalInlineEmbedContent(src)
    }

    private def insertAnchor(url: String,
                             cont: ContentBrowser,
                             importStatus: ImportStatus): Try[(String, Option[RequiredLibrary], ImportStatus)] = {
      val urlWithFragment = cont.getOpt("link_anchor").map(anchor => url.withFragment(anchor).toString)

      val htmlTag = HtmlTagGenerator.buildAnchor(urlWithFragment.getOrElse(url),
                                                 cont.get("link_text"),
                                                 cont.get("link_title_text"),
                                                 true)
      Success(s" $htmlTag", None, importStatus)
    }

    private def insertUnhandled(url: String,
                                cont: ContentBrowser,
                                importStatus: ImportStatus): Try[(String, Option[RequiredLibrary], ImportStatus)] = {
      insertAnchor(url, cont, importStatus) match {
        case Success((anchor, requiredLib, status)) =>
          val message =
            s"""Unhandled insertion method '${cont.get("insertion")}' on '${cont.get("link_text")}'. Defaulting to link."""
          logger.warn(message)
          Success(anchor, requiredLib, status.addMessage(message))
        case Failure(ex) => Failure(ex)
      }
    }
  }

}
