package com.earthspay.http

import com.earthspay.{NTPTime, TestWallet}
import com.earthspay.settings.EarthsSettings
import com.earthspay.api.http.ApiKeyNotValid

class DebugApiRouteSpec extends RouteSpec("/debug") with RestAPISettingsHelper with TestWallet with NTPTime {
  private val sampleConfig  = com.typesafe.config.ConfigFactory.load()
  private val earthsSettings = EarthsSettings.fromConfig(sampleConfig)
  private val configObject  = sampleConfig.root()
  private val route =
    DebugApiRoute(earthsSettings, ntpTime, null, null, null, null, null, null, null, null, null, null, null, null, null, configObject).route

  routePath("/configInfo") - {
    "requires api-key header" in {
      Get(routePath("/configInfo?full=true")) ~> route should produce(ApiKeyNotValid)
      Get(routePath("/configInfo?full=false")) ~> route should produce(ApiKeyNotValid)
    }
  }
}
