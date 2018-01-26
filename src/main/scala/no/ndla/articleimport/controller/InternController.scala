/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.articleimport.controller

import no.ndla.articleimport.auth.{Role, User}
import no.ndla.articleimport.model.domain.ImportStatus
import no.ndla.articleimport.service._
import org.json4s.{DefaultFormats, Formats}

import scala.util.{Failure, Success}

trait InternController {
  this: ExtractService
    with ConverterService
    with ExtractConvertStoreContent
    with User
    with Role =>
  val internController: InternController

  class InternController extends NdlaController {

    protected implicit override val jsonFormats: Formats = DefaultFormats
    private val RoleDraftWrite = "drafts:write"

    post("/import/:external_id") {
      authUser.assertHasId()
      authRole.assertHasRole(RoleDraftWrite)
      val externalId = params("external_id")
      val forceUpdate = booleanOrDefault("force_update", default = false)

      extractConvertStoreContent.processNode(externalId, ImportStatus.empty(forceUpdate)) match {
        case Success((content, status)) => status.addMessage(s"Successfully imported node $externalId: ${content.id}")
        case Failure(exc) => errorHandler(exc)
      }
    }

  }
}
