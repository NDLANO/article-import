/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.service

import java.util.Date

trait Clock {
  val clock: SystemClock

  class SystemClock {

    def now(): Date = {
      new Date()
    }
  }
}
