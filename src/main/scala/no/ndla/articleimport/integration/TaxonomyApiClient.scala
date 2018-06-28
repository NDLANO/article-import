/*
 * Part of NDLA article_import.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.integration

import com.typesafe.scalalogging.LazyLogging
import no.ndla.network.NdlaClient
import no.ndla.articleimport.ArticleImportProperties.ApiGatewayUrl
import no.ndla.articleimport.integration.TaxonomyApiClient.{Resource, Topic}
import no.ndla.network.model.HttpRequestException
import scalaj.http.Http

import scala.util.{Failure, Success, Try}

trait TaxonomyApiClient {
  this: NdlaClient =>
  val taxonomyApiClient: TaxonomyApiClient

  class TaxonomyApiClient extends LazyLogging {
    private val TaxonomyApiEndpoint = s"http://$ApiGatewayUrl/taxonomy/v1"

    def getResource(nid: String): Try[Option[Resource]] =
      get[Resource](s"$TaxonomyApiEndpoint/resources/urn:resource:1:$nid")

    def getTopic(nid: String): Try[Option[Topic]] =
      get[Topic](s"$TaxonomyApiEndpoint/topics/urn:topic:1:$nid")

    private def get[A](url: String, params: (String, String)*)(implicit mf: Manifest[A]): Try[Option[A]] = {
      ndlaClient.fetchWithForwardedAuth[A](Http(url).timeout(20000, 20000).params(params)) match {
        case Success(x)                                    => Success(Some(x))
        case Failure(ex: HttpRequestException) if ex.is404 => Success(None)
        case Failure(ex)                                   => Failure(ex)
      }
    }
  }

}

object TaxonomyApiClient {
  case class Resource(id: String, name: String, contentUri: Option[String], path: String)
  case class Topic(id: String, name: String, contentUri: Option[String], path: String)
}
