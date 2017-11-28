/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */


package no.ndla.articleimport.controller

import no.ndla.articleimport.service._
import org.json4s.{DefaultFormats, Formats}

import scala.util.{Failure, Success}

trait InternController {
  this: ExtractService
    with ConverterService
    with ExtractConvertStoreContent =>
  val internController: InternController

  class InternController extends NdlaController {

    protected implicit override val jsonFormats: Formats = DefaultFormats

    post("/import/:external_id") {
      val externalId = params("external_id")
      val forceUpdateArticle = booleanOrDefault("forceUpdate", false)

      extractConvertStoreContent.processNode(externalId, forceUpdateArticle) match {
        case Success((content, status)) => status.addMessage(s"Successfully imported node $externalId: ${content.id}")
        case Failure(exc) => errorHandler(exc)
      }
    }

  }
}
