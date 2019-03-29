package com.earthspay.it

import com.earthspay.api.http.assets.{SignedIssueV1Request, SignedIssueV2Request}
import com.earthspay.common.utils.{Base58, EitherExt2}
import com.earthspay.it.util._
import com.earthspay.state.DataEntry
import com.earthspay.transaction.assets.{IssueTransactionV1, IssueTransactionV2}
import com.earthspay.transaction.smart.script.{Script, ScriptCompiler}

package object sync {
  val smartFee: Long                   = 0.004.earths
  val minFee: Long                     = 0.001.earths
  val leasingFee: Long                 = 0.002.earths
  val issueFee: Long                   = 1.earths
  val burnFee: Long                    = 1.earths
  val sponsorFee: Long                 = 1.earths
  val setAssetScriptFee: Long          = 1.earths
  val setScriptFee: Long               = 0.01.earths
  val transferAmount: Long             = 10.earths
  val leasingAmount: Long              = transferAmount
  val issueAmount: Long                = transferAmount
  val massTransferFeePerTransfer: Long = 0.0005.earths
  val someAssetAmount: Long            = 9999999999999L
  val matcherFee: Long                 = 0.003.earths
  val orderFee: Long                   = matcherFee
  val smartMatcherFee: Long            = 0.007.earths
  val smartMinFee: Long                = minFee + smartFee

  def calcDataFee(data: List[DataEntry[_]]): Long = {
    val dataSize = data.map(_.toBytes.length).sum + 128
    if (dataSize > 1024) {
      minFee * (dataSize / 1024 + 1)
    } else minFee
  }

  def calcMassTransferFee(numberOfRecipients: Int): Long = {
    minFee + massTransferFeePerTransfer * (numberOfRecipients + 1)
  }

  val supportedVersions: List[Byte] = List(1, 2)

  val script: Script       = ScriptCompiler(s"""true""".stripMargin, isAssetScript = false).explicitGet()._1
  val scriptBase64: String = script.bytes.value.base64

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
