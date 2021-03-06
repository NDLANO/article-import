/*
 * Part of NDLA article_import.
 * Copyright (C) 2017 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport

import org.scalatest._
import org.mockito.scalatest.MockitoSugar
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

abstract class UnitSuite
    extends AnyFunSuite
    with Matchers
    with OptionValues
    with Inside
    with Inspectors
    with MockitoSugar
    with BeforeAndAfterEach
    with BeforeAndAfterAll {

  setEnv("NDLA_ENVIRONMENT", "local")

  setEnv("SEARCH_SERVER", "some-server")
  setEnv("SEARCH_REGION", "some-region")
  setEnv("RUN_WITH_SIGNED_SEARCH_REQUESTS", "false")

  setEnv("AUDIO_API_URL", "localhost:30014")
  setEnv("IMAGE_API_URL", "localhost:30001")

  setEnvIfAbsent("MIGRATION_HOST", "some-host")
  setEnvIfAbsent("MIGRATION_USER", "some-user")
  setEnvIfAbsent("MIGRATION_PASSWORD", "some-password")

  setEnv("NDLA_RED_USERNAME", "user")
  setEnv("NDLA_RED_PASSWORD", "pass")

  setEnv("NDLA_BRIGHTCOVE_ACCOUNT_ID", "some-account-id")
  setEnv("NDLA_BRIGHTCOVE_PLAYER_ID", "some-player-id")
  setEnv("SEARCH_INDEX_NAME", "article-integration-test-index")

  def setEnv(key: String, value: String) = env.put(key, value)

  def setEnvIfAbsent(key: String, value: String) = env.putIfAbsent(key, value)

  private def env = {
    val field = System.getenv().getClass.getDeclaredField("m")
    field.setAccessible(true)
    field
      .get(System.getenv())
      .asInstanceOf[java.util.Map[java.lang.String, java.lang.String]]
  }
}
