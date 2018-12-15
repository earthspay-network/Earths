package com.earthspay.generator

import cats.Show
import com.earthspay.account.PrivateKeyAccount
import com.earthspay.generator.OracleTransactionGenerator.Settings
import com.earthspay.generator.utils.Gen
import com.earthspay.it.util._
import com.earthspay.state._
import com.earthspay.transaction.smart.SetScriptTransaction
import com.earthspay.transaction.transfer.{TransferTransactionV2}
import com.earthspay.transaction.{DataTransaction, Transaction}

class OracleTransactionGenerator(settings: Settings, val accounts: Seq[PrivateKeyAccount]) extends TransactionGenerator {
  override def next(): Iterator[Transaction] = generate(settings).toIterator

  def generate(settings: Settings): Seq[Transaction] = {
    val oracle = accounts.last

    val scriptedAccount = accounts.head

    val script = Gen.oracleScript(oracle, settings.requiredData)

    val enoughFee = 0.005.earths

    val setScript: Transaction =
      SetScriptTransaction
        .selfSigned(1, scriptedAccount, Some(script), enoughFee, System.currentTimeMillis())
        .explicitGet()

    val setDataTx: Transaction = DataTransaction
      .selfSigned(1, oracle, settings.requiredData.toList, enoughFee, System.currentTimeMillis())
      .explicitGet()

    val transactions: List[Transaction] =
      List
        .fill(settings.transactions) {
          TransferTransactionV2
            .selfSigned(2, None, scriptedAccount, oracle, 1.earths, System.currentTimeMillis(), None, enoughFee, Array.emptyByteArray)
            .explicitGet()
        }

    setScript +: setDataTx +: transactions
  }
}

object OracleTransactionGenerator {
  final case class Settings(transactions: Int, requiredData: Set[DataEntry[_]])

  object Settings {
    implicit val toPrintable: Show[Settings] = { x =>
      s"Transactions: ${x.transactions}\n" +
        s"DataEntries: ${x.requiredData}\n"
    }
  }
}
