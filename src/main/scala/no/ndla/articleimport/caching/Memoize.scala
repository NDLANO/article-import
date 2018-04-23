/*
 * Part of NDLA article_import.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.caching

class Memoize[T, R](f: T => R, maxAgeMs: Long) extends (T => R) {
  case class CacheValue(value: R, lastUpdated: Long) {

    def isExpired: Boolean =
      lastUpdated + maxAgeMs <= System.currentTimeMillis()
  }

  private[this] var cache: Map[T, CacheValue] = Map.empty
  scala.collection.immutable.Map

  def apply(param: T): R = {
    cache.get(param) match {
      case Some(cachedValue) if !cachedValue.isExpired => cachedValue.value
      case _ =>
        val value = f(param)
        cache = cache.updated(param, CacheValue(value, System.currentTimeMillis()))
        value
    }
  }
}

object Memoize {

  def apply[T, R](f: T => R) =
    new Memoize[T, R](f, 1000 * 60 * 60) // default to 1 hour max age
}
