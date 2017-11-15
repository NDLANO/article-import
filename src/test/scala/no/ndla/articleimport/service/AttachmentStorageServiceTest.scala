/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.articleimport.service

import com.amazonaws.AmazonClientException
import com.amazonaws.services.s3.model.PutObjectRequest
import no.ndla.articleimport.{TestEnvironment, UnitSuite}
import org.mockito.Mockito._

import scala.util.Success

class AttachmentStorageServiceTest extends UnitSuite with TestEnvironment {
  override val attachmentStorageService = new AmazonStorageService

  test("That uploadFile returns None if upload fails") {
    val request = mock[PutObjectRequest]
    val key = "storagekey"

    when(amazonClient.putObject(request)).thenThrow(new AmazonClientException("Fail"))
    attachmentStorageService.uploadFile(request, key).isFailure should equal(true)
  }

  test("That uploadFile returns the second parameter on success") {
    val key = "storagekey"
    attachmentStorageService.uploadFile(mock[PutObjectRequest], key) should equal (Success(key))
  }
}
