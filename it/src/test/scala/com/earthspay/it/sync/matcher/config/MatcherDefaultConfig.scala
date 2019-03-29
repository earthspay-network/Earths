package com.earthspay.it.sync.matcher.config

import com.typesafe.config.{Config, ConfigFactory}
import com.earthspay.account.PrivateKeyAccount
import com.earthspay.api.http.assets.SignedIssueV1Request
import com.earthspay.transaction.assets.IssueTransactionV1
import com.earthspay.transaction.assets.exchange.AssetPair
import com.earthspay.it.sync._
import com.earthspay.matcher.market.MatcherActor
import com.earthspay.transaction.AssetId

import scala.util.Random

object MatcherDefaultConfig {

  import ConfigFactory._
  import com.earthspay.it.NodeConfigs._

  val ForbiddenAssetId = "FdbnAsset"
  val orderLimit       = 20

  val minerEnabled  = parseString(s"""
       |earths.miner.enable = yes
       |earths.miner.quorum = 0""".stripMargin)
  val minerDisabled = parseString("earths.miner.enable = no")
  val matcherConfig = parseString(s"""
                                     |earths.miner.enable = no
                                     |earths.matcher {
                                     |  enable = yes
                                     |  account = 3HmFkAoQRs4Y3PE2uR6ohN7wS4VqPBGKv7k
                                     |  bind-address = "0.0.0.0"
                                     |  order-match-tx-fee = 300000
                                     |  blacklisted-assets = ["$ForbiddenAssetId"]
                                     |  balance-watching.enable = yes
                                     |  rest-order-limit=$orderLimit
                                     |}""".stripMargin)

  val Configs: Seq[Config] = List(9, 5, 7)
    .map(Default)
    .zip(Seq(matcherConfig, minerDisabled, minerEnabled))
    .map { case (n, o) => o.withFallback(n) }

  def issueAssetPair(issuer: PrivateKeyAccount,
                     amountAssetDecimals: Byte,
                     priceAssetDecimals: Byte): (SignedIssueV1Request, SignedIssueV1Request, AssetPair) = {
    issueAssetPair(issuer, issuer, amountAssetDecimals, priceAssetDecimals)
  }

  def issueAssetPair(amountAssetIssuer: PrivateKeyAccount,
                     priceAssetIssuer: PrivateKeyAccount,
                     amountAssetDecimals: Byte,
                     priceAssetDecimals: Byte): (SignedIssueV1Request, SignedIssueV1Request, AssetPair) = {

    val issueAmountAssetTx: IssueTransactionV1 = IssueTransactionV1
      .selfSigned(
        sender = amountAssetIssuer,
        name = Random.nextString(4).getBytes(),
        description = Random.nextString(10).getBytes(),
        quantity = someAssetAmount,
        decimals = amountAssetDecimals,
        reissuable = false,
        fee = issueFee,
        timestamp = System.currentTimeMillis()
      )
      .right
      .get

    val issuePriceAssetTx: IssueTransactionV1 = IssueTransactionV1
      .selfSigned(
        sender = priceAssetIssuer,
        name = Random.nextString(4).getBytes(),
        description = Random.nextString(10).getBytes(),
        quantity = someAssetAmount,
        decimals = priceAssetDecimals,
        reissuable = false,
        fee = issueFee,
        timestamp = System.currentTimeMillis()
      )
      .right
      .get

    if (MatcherActor.compare(Some(issuePriceAssetTx.id().arr), Some(issueAmountAssetTx.id().arr)) < 0) {
      (createSignedIssueRequest(issueAmountAssetTx),
       createSignedIssueRequest(issuePriceAssetTx),
       AssetPair(
         amountAsset = Some(issueAmountAssetTx.id()),
         priceAsset = Some(issuePriceAssetTx.id())
       ))
    } else
      issueAssetPair(amountAssetIssuer, priceAssetIssuer, amountAssetDecimals, priceAssetDecimals)
  }

  def assetPairIssuePriceAsset(issuer: PrivateKeyAccount, amountAssetId: AssetId, priceAssetDecimals: Byte): (SignedIssueV1Request, AssetPair) = {

    val issuePriceAssetTx: IssueTransactionV1 = IssueTransactionV1
      .selfSigned(
        sender = issuer,
        name = Random.nextString(4).getBytes(),
        description = Random.nextString(10).getBytes(),
        quantity = someAssetAmount,
        decimals = priceAssetDecimals,
        reissuable = false,
        fee = issueFee,
        timestamp = System.currentTimeMillis()
      )
      .right
      .get

    if (MatcherActor.compare(Some(issuePriceAssetTx.id().arr), Some(amountAssetId.arr)) < 0) {
      (createSignedIssueRequest(issuePriceAssetTx),
       AssetPair(
         amountAsset = Some(amountAssetId),
         priceAsset = Some(issuePriceAssetTx.id())
       ))
    } else
      assetPairIssuePriceAsset(issuer, amountAssetId, priceAssetDecimals)
  }

}
