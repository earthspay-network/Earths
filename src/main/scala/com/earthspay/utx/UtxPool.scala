package com.earthspay.utx

import com.earthspay.account.Address
import com.earthspay.common.state.ByteStr
import com.earthspay.mining.MultiDimensionalMiningConstraint
import com.earthspay.state.{Diff, Portfolio}
import com.earthspay.transaction._

trait UtxPool extends AutoCloseable {
  self =>

  def putIfNew(tx: Transaction): Either[ValidationError, (Boolean, Diff)]

  def removeAll(txs: Traversable[Transaction]): Unit

  def spendableBalance(addr: Address, assetId: Option[AssetId]): Long

  def pessimisticPortfolio(addr: Address): Portfolio

  def all: Seq[Transaction]

  def size: Int

  def transactionById(transactionId: ByteStr): Option[Transaction]

  def packUnconfirmed(rest: MultiDimensionalMiningConstraint): (Seq[Transaction], MultiDimensionalMiningConstraint)

}
