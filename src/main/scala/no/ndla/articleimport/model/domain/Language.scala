/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.model.domain

import scala.annotation.tailrec

object Language {
  val DefaultLanguage = "nb"
  val UnknownLanguage = "unknown"
  val NoLanguage = ""
  val AllLanguages = "all"

  def findByLanguageOrBestEffort[P <: LanguageField[_]](sequence: Seq[P], lang: String): Option[P] = {
    @tailrec def findFirstLanguageMatching(sequence: Seq[P], lang: Seq[String]): Option[P] = {
      lang match {
        case Nil => sequence.headOption
        case head :: tail =>
          sequence.find(_.language == head) match {
            case Some(x) => Some(x)
            case None => findFirstLanguageMatching(sequence, tail)
          }
      }
    }

    findFirstLanguageMatching(sequence, lang :: DefaultLanguage :: Nil)
  }

  def languageOrUnknown(language: Option[String]): String = {
    language.filter(_.nonEmpty) match {
      case Some(x) => x
      case None => UnknownLanguage
    }
  }

}

