package com.earthspay.transaction

import com.earthspay.account.PublicKeyAccount

trait Authorized {
  val sender: PublicKeyAccount
}
