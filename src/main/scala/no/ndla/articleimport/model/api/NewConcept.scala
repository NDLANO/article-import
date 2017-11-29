/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.model.api

case class NewConcept(language: String, title: String, content: String, copyright: Option[Copyright])
