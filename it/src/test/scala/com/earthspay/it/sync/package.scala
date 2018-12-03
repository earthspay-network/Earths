package com.earthspay.it

import com.earthspay.state.DataEntry
import com.earthspay.it.util._

package object sync {
  val minFee                     = 0.001.earths
  val leasingFee                 = 0.002.earths
  val smartFee                   = 0.004.earths
  val issueFee                   = 1.earths
  val burnFee                    = 1.earths
  val sponsorFee                 = 1.earths
  val transferAmount             = 10.earths
  val leasingAmount              = transferAmount
  val issueAmount                = transferAmount
  val massTransferFeePerTransfer = 0.0005.earths
  val someAssetAmount            = 9999999999999l
  val matcherFee                 = 0.003.earths

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
}
