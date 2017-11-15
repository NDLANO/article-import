/*
 * Part of NDLA article_api.
 * Copyright (C) 2016 NDLA
 *
 * See LICENSE
 *
 */


import javax.servlet.ServletContext

import no.ndla.articleimport.ComponentRegistry.{healthController, internController}
import org.scalatra.LifeCycle

class ScalatraBootstrap extends LifeCycle {

  override def init(context: ServletContext) {
    context.mount(internController, "/intern")
    context.mount(healthController, "/health")
  }

}
