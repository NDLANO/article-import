/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.controller

import no.ndla.articleimport.auth.{Role, User}
import no.ndla.articleimport.model.domain.{ArticleType, ImportStatus}
import no.ndla.articleimport.service._
import org.json4s.ext.EnumNameSerializer
import org.json4s.{DefaultFormats, Formats}

import scala.util.{Failure, Success}

trait InternController {
  this: ExtractService with ConverterService with ExtractConvertStoreContent with User with NdlaController with Role =>
  val internController: InternController

  class InternController extends NdlaController {

    protected implicit override val jsonFormats: Formats = DefaultFormats + new EnumNameSerializer(ArticleType)
    private val RoleDraftWrite = "drafts:write"

    post("/import/:external_id") {
      authUser.assertHasId()
      authRole.assertHasRole(RoleDraftWrite)
      val externalId = params("external_id")
      val forceUpdate = booleanOrDefault("forceUpdate", default = false)
      val importId = paramOrNone("importId")

      extractConvertStoreContent.processNode(externalId, ImportStatus.empty(forceUpdate, importId)) match {
        case Success((content, status)) =>
          val msg = s"Successfully imported node $externalId: ${content.id}"
          if (status.messages.contains(msg)) status else status.addMessage(msg)
        case Failure(exc) =>
          logger.error(exc.getMessage)
          errorHandler(exc)
      }
    }
  }
}
