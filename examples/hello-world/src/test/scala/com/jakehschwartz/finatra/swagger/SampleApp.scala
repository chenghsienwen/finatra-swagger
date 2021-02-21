package com.jakehschwartz.finatra.swagger

import com.google.inject.Module
import com.twitter.finatra.http.HttpServer
import com.twitter.finatra.http.filters.CommonFilters
import com.twitter.finatra.http.routing.HttpRouter

object SampleAppMain extends SampleApp

class SampleApp extends HttpServer {

  override val name: String = "SampleApp"
  override protected def modules: Seq[Module] = Seq(SampleSwaggerModule)

  override def configureHttp(router: HttpRouter) {
    router
      .filter[CommonFilters]
      .add[SampleController]
      .add[DocsController]
  }
}
