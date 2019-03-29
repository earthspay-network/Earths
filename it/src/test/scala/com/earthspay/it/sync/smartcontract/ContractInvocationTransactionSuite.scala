package com.earthspay.it.sync.smartcontract

import com.earthspay.common.state.ByteStr
import com.earthspay.common.utils.EitherExt2
import com.earthspay.it.api.SyncHttpApi._
import com.earthspay.it.sync.{minFee, setScriptFee}
import com.earthspay.it.transactions.BaseTransactionSuite
import com.earthspay.it.util._
import com.earthspay.lang.v1.FunctionHeader
import com.earthspay.lang.v1.compiler.Terms.{CONST_BYTESTR, FUNCTION_CALL}
import com.earthspay.state._
import com.earthspay.transaction.smart.script.ScriptCompiler
import com.earthspay.transaction.smart.{ContractInvocationTransaction, SetScriptTransaction}
import com.earthspay.transaction.transfer._
import com.earthspay.transaction.{DataTransaction, Proofs}
import org.scalatest.{CancelAfterFailure, Ignore}
import play.api.libs.json.{JsNumber, Json}

@Ignore // ignored in v0.16
class ContractInvocationTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {

  private val contract = pkByAddress(firstAddress)
  private val caller   = pkByAddress(secondAddress)

  test("setup contract account with earths") {
    val tx =
      TransferTransactionV2
        .selfSigned(
          assetId = None,
          sender = sender.privateKey,
          recipient = contract,
          amount = 5.earths,
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

  test("setup caller account with earths") {
    val tx =
      TransferTransactionV2
        .selfSigned(
          assetId = None,
          sender = sender.privateKey,
          recipient = caller,
          amount = 5.earths,
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

  test("set contract to contract account") {
    val scriptText =
      """
        |
        | @Callable(inv)
        | func foo(a:ByteVector) = {
        |  WriteSet(List(DataEntry("a", a), DataEntry("sender", inv.caller.bytes)))
        | }
        | 
        | @Verifier(t)
        | func verify() = {
        |  true
        | }
        |
        |
        """.stripMargin

    val script = ScriptCompiler.contract(scriptText).explicitGet()
    val setScriptTransaction = SetScriptTransaction
      .selfSigned(contract, Some(script), setScriptFee, System.currentTimeMillis())
      .explicitGet()

    val setScriptId = sender
      .signedBroadcast(setScriptTransaction.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt)))
      .id

    nodes.waitForHeightAriseAndTxPresent(setScriptId)

    val acc0ScriptInfo = sender.addressScriptInfo(contract.address)

    acc0ScriptInfo.script.isEmpty shouldBe false
    acc0ScriptInfo.scriptText.isEmpty shouldBe false
    acc0ScriptInfo.script.get.startsWith("base64:") shouldBe true

    val json = Json.parse(sender.get(s"/transactions/info/$setScriptId").getResponseBody)
    (json \ "script").as[String].startsWith("base64:") shouldBe true
  }

  test("contract caller invokes a function on a contract") {
    val arg               = ByteStr(Array(42: Byte))
    val fc: FUNCTION_CALL = FUNCTION_CALL(FunctionHeader.User("foo"), List(CONST_BYTESTR(arg)))

    val tx =
      ContractInvocationTransaction
        .selfSigned(
          sender = caller,
          contractAddress = contract,
          fc = fc,
          p = None,
          timestamp = System.currentTimeMillis(),
          fee = 1.earths
        )
        .explicitGet()

    val contractInvocationId = sender
      .signedBroadcast(tx.json() + ("type" -> JsNumber(ContractInvocationTransaction.typeId.toInt)))
      .id

    nodes.waitForHeightAriseAndTxPresent(contractInvocationId)

    sender.getData(contract.address, "a") shouldBe BinaryDataEntry("a", arg)
    sender.getData(contract.address, "sender") shouldBe BinaryDataEntry("sender", caller.toAddress.bytes)
  }

  test("verifier works") {

    val tx =
      DataTransaction
        .create(
          sender = contract,
          data = List(StringDataEntry("a", "OOO")),
          feeAmount = 1.earths,
          timestamp = System.currentTimeMillis(),
          proofs = Proofs.empty
        )
        .explicitGet()

    val dataTxId = sender
      .signedBroadcast(tx.json() + ("type" -> JsNumber(DataTransaction.typeId.toInt)))
      .id

    nodes.waitForHeightAriseAndTxPresent(dataTxId)

    sender.getData(contract.address, "a") shouldBe StringDataEntry("a", "OOO")
  }
}
