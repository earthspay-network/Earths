package com.earthspay.state.diffs

import com.earthspay.state.{Diff, LeaseBalance, Portfolio}
import com.earthspay.transaction.ValidationError
import com.earthspay.transaction.smart.SetScriptTransaction

import scala.util.Right

object SetScriptTransactionDiff {
  def apply(height: Int)(tx: SetScriptTransaction): Either[ValidationError, Diff] = {
    Right(
      Diff(
        height = height,
        tx = tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty)),
        scripts = Map(tx.sender.toAddress    -> tx.script)
      ))
  }
}
