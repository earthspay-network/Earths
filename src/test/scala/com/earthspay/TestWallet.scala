package com.earthspay

import com.earthspay.settings.WalletSettings
import com.earthspay.wallet.Wallet

trait TestWallet {
  protected val testWallet = {
    val wallet = Wallet(WalletSettings(None, "123", None))
    wallet.generateNewAccounts(10)
    wallet
  }
}
