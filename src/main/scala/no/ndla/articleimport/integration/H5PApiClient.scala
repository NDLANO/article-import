/*
  * Part of NDLA article_import.
  * Copyright (C) 2017 NDLA
  *
  * See LICENSE
  *
  */

package no.ndla.articleimport.integration

import no.ndla.articleimport.ArticleImportProperties
import no.ndla.network.NdlaClient

import scalaj.http.{Http, HttpRequest}

trait H5PApiClient {
  this: NdlaClient =>
  val h5pApiClient: H5PApiClient

  class H5PApiClient {
    private val h5pApiClientGetNodeEndpoint =
      s"https://${ArticleImportProperties.H5PHost}/v1/ndla/node/%1s/view"

    def getViewFromOldId(nodeId: String): Option[String] = {
      implicit val formats = org.json4s.DefaultFormats
      val request: HttpRequest = Http(h5pApiClientGetNodeEndpoint.format(nodeId)).postForm
      ndlaClient.fetchWithForwardedAuth[h5pNode](request).toOption match {
        case Some(h5p) => Some(h5p.view)
        case None => None

      }
    }
  }

}

case class h5pNode(linkId: String, title: String, view: String, oembed: String)
