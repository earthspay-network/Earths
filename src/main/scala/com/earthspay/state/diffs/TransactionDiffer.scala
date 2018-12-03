package com.earthspay.state.diffs

import com.earthspay.settings.FunctionalitySettings
import com.earthspay.state._
import com.earthspay.transaction.ValidationError.UnsupportedTransactionType
import com.earthspay.transaction._
import com.earthspay.transaction.assets._
import com.earthspay.transaction.assets.exchange.ExchangeTransaction
import com.earthspay.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.earthspay.transaction.smart.{SetScriptTransaction, Verifier}
import com.earthspay.transaction.transfer._

object TransactionDiffer {

  case class TransactionValidationError(cause: ValidationError, tx: Transaction) extends ValidationError

  def apply(settings: FunctionalitySettings, prevBlockTimestamp: Option[Long], currentBlockTimestamp: Long, currentBlockHeight: Int)(
      blockchain: Blockchain,
      tx: Transaction): Either[ValidationError, Diff] = {
    for {
      _ <- Verifier(blockchain, currentBlockHeight)(tx)
      _ <- CommonValidation.disallowTxFromFuture(settings, currentBlockTimestamp, tx)
      _ <- CommonValidation.disallowTxFromPast(prevBlockTimestamp, tx)
      _ <- CommonValidation.disallowBeforeActivationTime(blockchain, currentBlockHeight, tx)
      _ <- CommonValidation.disallowDuplicateIds(blockchain, settings, currentBlockHeight, tx)
      _ <- CommonValidation.disallowSendingGreaterThanBalance(blockchain, settings, currentBlockTimestamp, tx)
      _ <- CommonValidation.checkFee(blockchain, settings, currentBlockHeight, tx)
      diff <- tx match {
        case gtx: GenesisTransaction      => GenesisTransactionDiff(currentBlockHeight)(gtx)
        case ptx: PaymentTransaction      => PaymentTransactionDiff(blockchain, currentBlockHeight, settings, currentBlockTimestamp)(ptx)
        case itx: IssueTransaction        => AssetTransactionsDiff.issue(currentBlockHeight)(itx)
        case rtx: ReissueTransaction      => AssetTransactionsDiff.reissue(blockchain, settings, currentBlockTimestamp, currentBlockHeight)(rtx)
        case btx: BurnTransaction         => AssetTransactionsDiff.burn(blockchain, currentBlockHeight)(btx)
        case ttx: TransferTransaction     => TransferTransactionDiff(blockchain, settings, currentBlockTimestamp, currentBlockHeight)(ttx)
        case mtx: MassTransferTransaction => MassTransferTransactionDiff(blockchain, currentBlockTimestamp, currentBlockHeight)(mtx)
        case ltx: LeaseTransaction        => LeaseTransactionsDiff.lease(blockchain, currentBlockHeight)(ltx)
        case ltx: LeaseCancelTransaction  => LeaseTransactionsDiff.leaseCancel(blockchain, settings, currentBlockTimestamp, currentBlockHeight)(ltx)
        case etx: ExchangeTransaction     => ExchangeTransactionDiff(blockchain, currentBlockHeight)(etx)
        case atx: CreateAliasTransaction  => CreateAliasTransactionDiff(blockchain, currentBlockHeight)(atx)
        case dtx: DataTransaction         => DataTransactionDiff(blockchain, currentBlockHeight)(dtx)
        case sstx: SetScriptTransaction   => SetScriptTransactionDiff(currentBlockHeight)(sstx)
        case stx: SponsorFeeTransaction   => AssetTransactionsDiff.sponsor(blockchain, settings, currentBlockTimestamp, currentBlockHeight)(stx)
        case _                            => Left(UnsupportedTransactionType)
      }
      positiveDiff <- BalanceDiffValidation(blockchain, currentBlockHeight, settings)(diff)
    } yield positiveDiff
  }.left.map(TransactionValidationError(_, tx))
}
