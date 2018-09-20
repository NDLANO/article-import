/*
 * Part of NDLA article-import.
 * Copyright (C) 2018 NDLA
 *
 * See LICENSE
 */

package no.ndla.articleimport.caching
import org.mockito.Mockito._

import no.ndla.articleimport.UnitSuite

class MemoizeTest extends UnitSuite {

  class Target {
    def targetMethod(a: Int): String = "Hei"
  }

  test("That an uncached value will do an actual call") {
    val targetMock = mock[Target]
    val memoizedTarget = new Memoize[Int, String](targetMock.targetMethod, Long.MaxValue)

    when(targetMock.targetMethod(1)).thenReturn("Hello from mock")
    memoizedTarget(1) should equal("Hello from mock")
    verify(targetMock, times(1)).targetMethod(1)
  }

  test("That a cached value will not forward the call to the target") {
    val targetMock = mock[Target]
    val memoizedTarget = new Memoize[Int, String](targetMock.targetMethod, Long.MaxValue)

    when(targetMock.targetMethod(1)).thenReturn("Hello from mock")
    Seq(1 to 10).foreach(_ => {
      memoizedTarget(1) should equal("Hello from mock")
    })
    verify(targetMock, times(1)).targetMethod(1)
  }

  test("That the cache is invalidated after cacheMaxAge") {
    val cacheMaxAgeInMs = 20
    val targetMock = mock[Target]
    val memoizedTarget = new Memoize[Int, String](targetMock.targetMethod, cacheMaxAgeInMs)

    when(targetMock.targetMethod(1)).thenReturn("Hello from mock")

    memoizedTarget(1) should equal("Hello from mock")
    memoizedTarget(1) should equal("Hello from mock")
    Thread.sleep(cacheMaxAgeInMs)
    memoizedTarget(1) should equal("Hello from mock")
    memoizedTarget(1) should equal("Hello from mock")

    verify(targetMock, times(2)).targetMethod(1)
  }
}
