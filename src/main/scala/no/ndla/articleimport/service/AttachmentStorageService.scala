/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service

import com.amazonaws.AmazonServiceException
import com.amazonaws.services.s3.model._
import com.typesafe.scalalogging.LazyLogging
import io.lemonlabs.uri.dsl._
import no.ndla.articleimport.ArticleImportProperties.{NdlaRedHost, NdlaRedPassword, NdlaRedUsername}
import no.ndla.articleimport.integration.AmazonClient
import no.ndla.articleimport.model.domain.ContentFilMeta
import scalaj.http.Http

import scala.util.Try

trait AttachmentStorageService {
  this: AmazonClient =>
  val attachmentStorageService: AmazonStorageService

  class AmazonStorageService extends LazyLogging {

    def uploadFileFromUrl(nodeId: String, filMeta: ContentFilMeta): Try[String] = {
      val storageKey = s"$nodeId/${filMeta.fileName}"
      val metaData = new ObjectMetadata()
      metaData.setContentType(filMeta.mimeType)
      metaData.setContentLength(filMeta.fileSize.toLong)

      val request =
        if (filMeta.url.hostOption.exists(_.toString == NdlaRedHost))
          Http(filMeta.url).auth(NdlaRedUsername, NdlaRedPassword)
        else
          Http(filMeta.url)

      request
        .execute(parser = is =>
          uploadFile(new PutObjectRequest(attachmentStorageName, storageKey, is, metaData), storageKey))
        .body
    }

    def uploadFile(request: PutObjectRequest, storageKey: String): Try[String] =
      Try(amazonClient.putObject(request)).map(_ => storageKey)

    def contains(storageKey: String): Boolean = {
      try {
        val s3Object = Option(amazonClient.getObject(new GetObjectRequest(attachmentStorageName, storageKey)))
        s3Object match {
          case Some(obj) => {
            obj.close()
            true
          }
          case None => false
        }
      } catch {
        case ase: AmazonServiceException =>
          if (ase.getErrorCode == "NoSuchKey") false else throw ase
      }
    }
  }
}
