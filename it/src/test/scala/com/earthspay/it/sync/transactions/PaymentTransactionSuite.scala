package com.earthspay.it.sync.transactions

import com.earthspay.it.api.SyncHttpApi._
import com.earthspay.it.api.PaymentRequest
import com.earthspay.it.transactions.BaseTransactionSuite
import com.earthspay.it.util._
import org.scalatest.prop.TableDrivenPropertyChecks

class PaymentTransactionSuite extends BaseTransactionSuite with TableDrivenPropertyChecks {

  private val paymentAmount = 5.earths
  private val defaulFee     = 1.earths

  test("earths payment changes earths balances and eff.b.") {

    val (firstBalance, firstEffBalance)   = notMiner.accountBalances(firstAddress)
    val (secondBalance, secondEffBalance) = notMiner.accountBalances(secondAddress)

    val transferId = sender.payment(firstAddress, secondAddress, paymentAmount, defaulFee).id
    nodes.waitForHeightAriseAndTxPresent(transferId)
    notMiner.assertBalances(firstAddress, firstBalance - paymentAmount - defaulFee, firstEffBalance - paymentAmount - defaulFee)
    notMiner.assertBalances(secondAddress, secondBalance + paymentAmount, secondEffBalance + paymentAmount)
  }

  val payment = PaymentRequest(5.earths, 1.earths, firstAddress, secondAddress)
  val endpoints =
    Table("/earths/payment/signature", "/earths/create-signed-payment", "/earths/external-payment", "/earths/broadcast-signed-payment")
  forAll(endpoints) { (endpoint: String) =>
    test(s"obsolete endpoints respond with BadRequest. Endpoint:$endpoint") {
      val errorMessage = "This API is no longer supported"
      assertBadRequestAndMessage(sender.postJson(endpoint, payment), errorMessage)
    }
  }
}
