/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.integration

import no.ndla.articleimport.ArticleImportProperties
import no.ndla.network.NdlaClient

import scala.util.{Failure, Success, Try}
import scalaj.http.Http

trait AudioApiClient {
  this: NdlaClient =>
  val audioApiClient: AudioApiClient

  class AudioApiClient {
    private val AudioMetaInternEndpoint = s"http://${ArticleImportProperties.AudioHost}/intern"
    private val AudioMetaFromExternalIdEndpoint = s"$AudioMetaInternEndpoint/:external_id"
    private val ImportAudioEndpoint = s"$AudioMetaInternEndpoint/import/:external_id"
    private val AudioHealthEndpoint = s"http://${ArticleImportProperties.AudioHost}/health"

    def getOrImportAudio(externalId: String): Try[Long] =
      getAudioFromExternalId(externalId) orElse importAudio(externalId)

    def getAudioFromExternalId(externalId: String): Try[Long] = {
      val request = Http(AudioMetaFromExternalIdEndpoint.replace(":external_id", externalId))
      ndlaClient.fetch[AudioApiMetaInformation](request).map(_.id)
    }

    def importAudio(externalId: String): Try[Long] = {
      val second = 1000
      val request = Http(ImportAudioEndpoint.replace(":external_id", externalId)).timeout(20 * second, 20 * second).postForm
      ndlaClient.fetch[AudioApiMetaInformation](request).map(_.id)
    }

    def isHealthy: Boolean = {
      Try(Http(AudioHealthEndpoint).execute()) match {
        case Success(resp) => resp.isSuccess
        case _ => false
      }
    }
  }
}

case class AudioApiMetaInformation(id: Long)
