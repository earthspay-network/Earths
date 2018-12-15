package com.earthspay

import com.earthspay.settings.WalletSettings
import com.earthspay.wallet.Wallet

trait TestWallet {
  protected val testWallet: Wallet = {
    val wallet = Wallet(WalletSettings(None, Some("123"), None))
    wallet.generateNewAccounts(10)
    wallet
  }
}
