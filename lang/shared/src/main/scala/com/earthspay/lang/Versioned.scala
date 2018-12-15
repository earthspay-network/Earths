package com.earthspay.lang

trait Versioned {
  type Ver <: ScriptVersion
  val version: Ver
}
