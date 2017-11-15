/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.model.domain

import java.util.Date

case class ImportError(description: String = "Error during import", messages: Seq[String], occuredAt: Date = new Date())

