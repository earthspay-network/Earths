package com.earthspay.state.diffs

import com.earthspay.features.BlockchainFeatures
import com.earthspay.features.FeatureProvider._
import com.earthspay.state.{Blockchain, Diff, LeaseBalance, Portfolio}
import com.earthspay.transaction.ValidationError
import com.earthspay.transaction.smart.SetScriptTransaction

import com.earthspay.transaction.ValidationError
import com.earthspay.transaction.ValidationError.GenericError
import com.earthspay.lang.v1.DenyDuplicateVarNames
import com.earthspay.utils.varNames

import scala.util.Right

object SetScriptTransactionDiff {
  def apply(blockchain: Blockchain, height: Int)(tx: SetScriptTransaction): Either[ValidationError, Diff] = {
    val scriptOpt = tx.script
    for {
      _ <- scriptOpt.fold(Right(()): Either[ValidationError, Unit]) { script =>
        if (blockchain.isFeatureActivated(BlockchainFeatures.SmartAccountTrading, height)) {
          Right(())
        } else {
          val version = script.version
          DenyDuplicateVarNames(version, varNames(version), script.expr).left.map(GenericError.apply)
        }
      }
    } yield {
      Diff(
        height = height,
        tx = tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty)),
        scripts = Map(tx.sender.toAddress    -> scriptOpt)
      )
    }
  }
}
