/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.articleimport

import com.typesafe.scalalogging.LazyLogging
import no.ndla.validation.ResourceType
import no.ndla.network.secrets.PropertyKeys
import no.ndla.network.secrets.Secrets.readSecrets
import no.ndla.network.Domains

import scala.util.Properties._
import scala.util.{Failure, Success}

object ArticleImportProperties extends LazyLogging {
  val SecretsFile = "article-api.secrets"

  val ApplicationPort = propOrElse("APPLICATION_PORT", "80").toInt
  val ContactEmail = "christergundersen@ndla.no"
  val Environment = propOrElse("NDLA_ENVIRONMENT", "local")

  val AttachmentStorageName = s"$Environment.article-attachments.ndla"

  val TopicAPIUrl = "http://api.topic.ndla.no/rest/v1/keywords/?filter[node]=ndlanode_"
  val MigrationHost = prop("MIGRATION_HOST")
  val MigrationUser = prop("MIGRATION_USER")
  val MigrationPassword = prop("MIGRATION_PASSWORD")

  val CorrelationIdKey = "correlationID"
  val CorrelationIdHeader = "X-Correlation-ID"
  val DraftHost = propOrElse("DRAFT_API_HOST", "draft-api.ndla-local")
  val AudioHost = propOrElse("AUDIO_API_HOST", "audio-api.ndla-local")
  val ImageHost = propOrElse("IMAGE_API_HOST", "image-api.ndla-local")

  val nodeTypeBegrep: String = "begrep"
  val nodeTypeVideo: String = "video"
  val nodeTypeH5P: String = "h5p_content"
  val supportedContentTypes = Set("fagstoff", "oppgave", "veiledning", "aktualitet", "emneartikkel", nodeTypeBegrep, nodeTypeVideo, nodeTypeH5P)


  // When converting a content node, the converter may run several times over the content to make sure
  // everything is converted. This value defines a maximum number of times the converter runs on a node
  val maxConvertionRounds = 5

  lazy val Domain = Domains.get(Environment)

  val externalApiUrls = Map(
    ResourceType.Image.toString -> s"$Domain/image-api/v2/images",
    "raw-image" -> s"$Domain/image-api/raw/id",
    ResourceType.Audio.toString -> s"$Domain/audio-api/v1/audio"
  )

  val NDLABrightcoveAccountId = prop("NDLA_BRIGHTCOVE_ACCOUNT_ID")
  val NDLABrightcovePlayerId = prop("NDLA_BRIGHTCOVE_PLAYER_ID")
  val EnableJoubelH5POembed = booleanProp("ENABLE_JOUBEL_H5P_OEMBED")

  val H5PResizerScriptUrl = "//ndla.no/sites/all/modules/h5p/library/js/h5p-resizer.js"
  val NDLABrightcoveVideoScriptUrl = s"//players.brightcove.net/$NDLABrightcoveAccountId/${NDLABrightcovePlayerId}_default/index.min.js"
  val NRKVideoScriptUrl = Seq("//www.nrk.no/serum/latest/js/video_embed.js", "//nrk.no/serum/latest/js/video_embed.js")

  lazy val secrets = readSecrets(SecretsFile) match {
     case Success(values) => values
     case Failure(exception) => throw new RuntimeException(s"Unable to load remote secrets from $SecretsFile", exception)
   }

  def booleanProp(key: String): Boolean = prop(key).toBoolean

  def prop(key: String): String = propOrElse(key, throw new RuntimeException(s"Unable to load property $key"))

  def propOrElse(key: String, default: => String): String = {
    secrets.get(key).flatten match {
      case Some(secret) => secret
      case None =>
        envOrNone(key) match {
          case Some(env) => env
          case None => default
        }
    }
  }
}
