package com.earthspay.settings

import java.io.File

import com.earthspay.common.state.ByteStr

case class WalletSettings(file: Option[File], password: Option[String], seed: Option[ByteStr])
