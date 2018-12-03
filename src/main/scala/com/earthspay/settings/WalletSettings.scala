package com.earthspay.settings

import java.io.File

import com.earthspay.state.ByteStr

case class WalletSettings(file: Option[File], password: String, seed: Option[ByteStr])
