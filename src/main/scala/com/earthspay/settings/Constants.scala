package com.earthspay.settings

import com.earthspay.Version
import com.earthspay.utils.ScorexLogging

/**
  * System constants here.
  */
object Constants extends ScorexLogging {
  val ApplicationName = "earths"
  val AgentName       = s"Earths v${Version.VersionString}"

  val UnitsInEarth = 100000000L
  val TotalEarths  = 92233720368L
}
