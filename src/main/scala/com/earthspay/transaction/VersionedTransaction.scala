package com.earthspay.transaction

trait VersionedTransaction {
  def version: Byte
}
