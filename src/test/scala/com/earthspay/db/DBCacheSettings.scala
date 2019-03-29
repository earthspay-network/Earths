package com.earthspay.db
import com.typesafe.config.ConfigFactory
import com.earthspay.settings.EarthsSettings

trait DBCacheSettings {
  lazy val maxCacheSize: Int = {
    val settings = EarthsSettings.fromConfig(ConfigFactory.load())
    settings.maxCacheSize
  }
}
