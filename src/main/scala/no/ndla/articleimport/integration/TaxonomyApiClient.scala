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
import no.ndla.network.model.HttpRequestException
import scalaj.http.Http

import scala.util.{Failure, Success, Try}

trait TaxonomyApiClient {
  this: NdlaClient =>
  val taxonomyApiClient: TaxonomyApiClient

  class TaxonomyApiClient extends LazyLogging {
    private val TaxonomyApiEndpoint = s"http://$ApiGatewayUrl/taxonomy/v1"

    def existsInTaxonomy(nid: String): Try[Boolean] = {
      val res = getResource(nid)
      val top = getTopic(nid)

      res.orElse(top) match {
        case Success(_)                                    => Success(true)
        case Failure(ex: HttpRequestException) if ex.is404 => Success(true)
        case Failure(ex)                                   => Failure(ex)
      }
    }

    private def getResource(nid: String): Try[Resource] =
      get[Resource](s"$TaxonomyApiEndpoint/resources/urn:resource:1:$nid")

    private def getTopic(nid: String): Try[Resource] =
      get[Resource](s"$TaxonomyApiEndpoint/topics/urn:topic:1:$nid")

    private def get[A](url: String, params: (String, String)*)(implicit mf: Manifest[A]): Try[A] = {
      ndlaClient.fetchWithForwardedAuth[A](Http(url).timeout(20000, 20000).params(params))
    }
  }

}

case class Resource(id: String, name: String, contentUri: Option[String], path: String)
