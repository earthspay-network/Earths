package com.earthspay.it

import com.earthspay.api.http.assets.{SignedIssueV1Request, SignedIssueV2Request}
import com.earthspay.it.util._
import com.earthspay.state.DataEntry
import com.earthspay.transaction.assets.{IssueTransactionV1, IssueTransactionV2}
import com.earthspay.transaction.smart.script.ScriptCompiler
import com.earthspay.utils.Base58

package object sync {
  val smartFee                   = 0.004.earths
  val minFee                     = 0.001.earths
  val leasingFee                 = 0.002.earths
  val issueFee                   = 1.earths
  val burnFee                    = 1.earths
  val sponsorFee                 = 1.earths
  val setAssetScriptFee          = 1.earths
  val setScriptFee               = 0.01.earths
  val transferAmount             = 10.earths
  val leasingAmount              = transferAmount
  val issueAmount                = transferAmount
  val massTransferFeePerTransfer = 0.0005.earths
  val someAssetAmount            = 9999999999999l
  val matcherFee                 = 0.003.earths
  val orderFee                   = matcherFee
  val smartMatcherFee            = 0.007.earths
  val smartMinFee                = minFee + smartFee

  def calcDataFee(data: List[DataEntry[_]]): Long = {
    val dataSize = data.map(_.toBytes.length).sum + 128
    if (dataSize > 1024) {
      minFee * (dataSize / 1024 + 1)
    } else minFee
  }

  def calcMassTransferFee(numberOfRecipients: Int): Long = {
    minFee + massTransferFeePerTransfer * (numberOfRecipients + 1)
  }

  val supportedVersions = List(null, "2") //sign and broadcast use default for V1

  val script       = ScriptCompiler(s"""true""".stripMargin, isAssetScript = false).explicitGet()._1
  val scriptBase64 = script.bytes.value.base64

  val errNotAllowedByToken = "Transaction is not allowed by token-script"

  def createSignedIssueRequest(tx: IssueTransactionV1): SignedIssueV1Request = {
    import tx._
    SignedIssueV1Request(
      Base58.encode(tx.sender.publicKey),
      new String(name),
      new String(description),
      quantity,
      decimals,
      reissuable,
      fee,
      timestamp,
      signature.base58
    )
  }

  def createSignedIssueRequest(tx: IssueTransactionV2): SignedIssueV2Request = {
    import tx._
    SignedIssueV2Request(
      2.toByte,
      Base58.encode(tx.sender.publicKey),
      new String(name),
      new String(description),
      quantity,
      decimals,
      reissuable,
      fee,
      timestamp,
      proofs.proofs.map(_.toString),
      tx.script.map(_.bytes().base64)
    )
  }

}
