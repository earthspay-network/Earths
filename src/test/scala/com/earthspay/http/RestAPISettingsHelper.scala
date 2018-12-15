package com.earthspay.http

import com.typesafe.config.ConfigFactory
import com.earthspay.crypto
import com.earthspay.settings.RestAPISettings
import com.earthspay.utils.Base58

trait RestAPISettingsHelper {
  def apiKey: String = "test_api_key"

  lazy val MaxTransactionsPerRequest = 10000
  lazy val MaxAddressesPerRequest    = 10000

  lazy val restAPISettings = {
    val keyHash = Base58.encode(crypto.secureHash(apiKey.getBytes()))
    RestAPISettings.fromConfig(
      ConfigFactory
        .parseString(
          s"""
             |earths.rest-api {
             |  api-key-hash = $keyHash
             |  transactions-by-address-limit = $MaxTransactionsPerRequest
             |  distribution-by-address-limit = $MaxAddressesPerRequest
             |}
           """.stripMargin
        )
        .withFallback(ConfigFactory.load()))
  }
}
