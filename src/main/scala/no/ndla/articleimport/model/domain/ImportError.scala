/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.model.domain

import java.util.Date

case class ImportError(description: String = "Error during import", messages: Set[ImportMessages], occuredAt: Date = new Date())
case class ImportMessages(nids: Set[String], messages: Set[String])

