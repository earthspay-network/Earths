package com.earthspay.it.sync.smartcontract

import com.earthspay.crypto
import com.earthspay.it.api.SyncHttpApi._
import com.earthspay.it.sync.{minFee, transferAmount}
import com.earthspay.it.transactions.BaseTransactionSuite
import com.earthspay.it.util._
import com.earthspay.state._
import org.scalatest.CancelAfterFailure
import play.api.libs.json.{JsNumber, Json}
import com.earthspay.transaction.Proofs
import com.earthspay.transaction.smart.SetScriptTransaction
import com.earthspay.transaction.smart.script.ScriptCompiler
import com.earthspay.transaction.transfer._

class SetScriptTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {
  private val fourthAddress: String = sender.createAddress()

  private val acc0 = pkByAddress(firstAddress)
  private val acc1 = pkByAddress(secondAddress)
  private val acc2 = pkByAddress(thirdAddress)
  private val acc3 = pkByAddress(fourthAddress)

  test("setup acc0 with 1 earths") {
    val tx =
      TransferTransactionV2
        .selfSigned(
          version = 2,
          assetId = None,
          sender = sender.privateKey,
          recipient = acc0,
          amount = 3 * transferAmount + 3 * (0.00001.earths + 0.00002.earths), // Script fee
          timestamp = System.currentTimeMillis(),
          feeAssetId = None,
          feeAmount = minFee,
          attachment = Array.emptyByteArray
        )
        .explicitGet()

    val transferId = sender
      .signedBroadcast(tx.json() + ("type" -> JsNumber(TransferTransactionV2.typeId.toInt)))
      .id
    nodes.waitForHeightAriseAndTxPresent(transferId)
  }

  test("set acc0 as 2of2 multisig") {
    val scriptText = s"""

        let A = base58'${ByteStr(acc1.publicKey)}'
        let B = base58'${ByteStr(acc2.publicKey)}'

        let AC = sigVerify(tx.bodyBytes,tx.proofs[0],A)
        let BC = sigVerify(tx.bodyBytes,tx.proofs[1],B)

         AC && BC

      """.stripMargin

    val script = ScriptCompiler(scriptText).explicitGet()._1
    val setScriptTransaction = SetScriptTransaction
      .selfSigned(SetScriptTransaction.supportedVersions.head, acc0, Some(script), minFee, System.currentTimeMillis())
      .explicitGet()

    val setScriptId = sender
      .signedBroadcast(setScriptTransaction.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt)))
      .id

    nodes.waitForHeightAriseAndTxPresent(setScriptId)

    val acc0ScriptInfo = sender.addressScriptInfo(acc0.address)

    acc0ScriptInfo.script.isEmpty shouldBe false
    acc0ScriptInfo.scriptText.isEmpty shouldBe false
    acc0ScriptInfo.script.get.startsWith("base64:") shouldBe true

    val json = Json.parse(sender.get(s"/transactions/info/$setScriptId").getResponseBody)
    (json \ "script").as[String].startsWith("base64:") shouldBe true
  }

  test("can't send from acc0 using old pk") {
    val tx =
      TransferTransactionV2
        .selfSigned(
          version = 2,
          assetId = None,
          sender = acc0,
          recipient = acc3,
          amount = transferAmount,
          timestamp = System.currentTimeMillis(),
          feeAssetId = None,
          feeAmount = minFee + 0.00001.earths + 0.00002.earths,
          attachment = Array.emptyByteArray
        )
        .explicitGet()
    assertBadRequest(sender.signedBroadcast(tx.json() + ("type" -> JsNumber(TransferTransactionV2.typeId.toInt))))
  }

  test("can send from acc0 using multisig of acc1 and acc2") {
    val unsigned =
      TransferTransactionV2
        .create(
          version = 2,
          assetId = None,
          sender = acc0,
          recipient = acc3,
          amount = transferAmount,
          timestamp = System.currentTimeMillis(),
          feeAssetId = None,
          feeAmount = minFee + 0.004.earths,
          attachment = Array.emptyByteArray,
          proofs = Proofs.empty
        )
        .explicitGet()
    val sig1 = ByteStr(crypto.sign(acc1, unsigned.bodyBytes()))
    val sig2 = ByteStr(crypto.sign(acc2, unsigned.bodyBytes()))

    val signed = unsigned.copy(proofs = Proofs(Seq(sig1, sig2)))

    val versionedTransferId =
      sender.signedBroadcast(signed.json() + ("type" -> JsNumber(TransferTransactionV2.typeId.toInt))).id

    nodes.waitForHeightAriseAndTxPresent(versionedTransferId)
  }

  test("can clear script at acc0") {
    val unsigned = SetScriptTransaction
      .create(
        version = SetScriptTransaction.supportedVersions.head,
        sender = acc0,
        script = None,
        fee = minFee + 0.004.earths,
        timestamp = System.currentTimeMillis(),
        proofs = Proofs.empty
      )
      .explicitGet()
    val sig1 = ByteStr(crypto.sign(acc1, unsigned.bodyBytes()))
    val sig2 = ByteStr(crypto.sign(acc2, unsigned.bodyBytes()))

    val signed = unsigned.copy(proofs = Proofs(Seq(sig1, sig2)))
    val clearScriptId = sender
      .signedBroadcast(signed.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt)))
      .id

    nodes.waitForHeightAriseAndTxPresent(clearScriptId)
  }

  test("can send using old pk of acc0") {
    val tx =
      TransferTransactionV2
        .selfSigned(
          version = 2,
          assetId = None,
          sender = acc0,
          recipient = acc3,
          amount = transferAmount,
          timestamp = System.currentTimeMillis(),
          feeAssetId = None,
          feeAmount = minFee + 0.004.earths,
          attachment = Array.emptyByteArray
        )
        .explicitGet()
    val txId = sender.signedBroadcast(tx.json() + ("type" -> JsNumber(TransferTransactionV2.typeId.toInt))).id
    nodes.waitForHeightAriseAndTxPresent(txId)
  }
}
