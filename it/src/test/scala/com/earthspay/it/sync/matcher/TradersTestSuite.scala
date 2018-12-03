package com.earthspay.it.sync.matcher

import com.typesafe.config.Config
import com.earthspay.it.ReportingTestName
import com.earthspay.it.api.SyncHttpApi._
import com.earthspay.it.api.SyncMatcherHttpApi._
import com.earthspay.it.sync._
import com.earthspay.it.sync.matcher.config.MatcherDefaultConfig._
import com.earthspay.it.transactions.NodesFromDocker
import com.earthspay.it.util._
import com.earthspay.matcher.market.MatcherActor
import com.earthspay.matcher.model.MatcherModel.Price
import com.earthspay.state.ByteStr
import com.earthspay.transaction.assets.exchange.{AssetPair, Order, OrderType}
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

class TradersTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with CancelAfterFailure with NodesFromDocker with ReportingTestName {

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode    = nodes.head
  private def aliceNode      = nodes(1)
  private def bobNode        = nodes(2)
  private val TransactionFee = 300000

  "Verifications of tricky ordering cases" - {
    // Alice issues new asset
    val aliceAsset =
      aliceNode.issue(aliceNode.address, "AliceCoin", "AliceCoin for matcher's tests", someAssetAmount, 0, reissuable = false, 100000000L).id
    nodes.waitForHeightAriseAndTxPresent(aliceAsset)

    // val aliceEarthsPair = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)

    // Wait for balance on Alice's account
    aliceNode.assertAssetBalance(aliceNode.address, aliceAsset, someAssetAmount)
    matcherNode.assertAssetBalance(matcherNode.address, aliceAsset, 0)
    bobNode.assertAssetBalance(bobNode.address, aliceAsset, 0)

    // Bob issues a new asset
    val bobAssetQuantity = 10000
    val bobNewAsset      = bobNode.issue(bobNode.address, "BobCoin3", "Bob's asset", bobAssetQuantity, 0, false, 100000000L).id
    nodes.waitForHeightAriseAndTxPresent(bobNewAsset)
    val bobAssetId   = ByteStr.decodeBase58(bobNewAsset).get
    val aliceAssetId = ByteStr.decodeBase58(aliceAsset).get

    val bobEarthsPair = AssetPair(
      amountAsset = Some(bobAssetId),
      priceAsset = None
    )

    val twoAssetsPair =
      if (MatcherActor.compare(Some(bobAssetId.arr), Some(aliceAssetId.arr)) < 0)
        AssetPair(
          amountAsset = Some(aliceAssetId),
          priceAsset = Some(bobAssetId)
        )
      else
        AssetPair(
          amountAsset = Some(bobAssetId),
          priceAsset = Some(aliceAssetId)
        )

    nodes.waitForHeightArise()
    bobNode.assertAssetBalance(bobNode.address, bobNewAsset, bobAssetQuantity)

    "matcher should respond with Public key" in {
      matcherNode.matcherGet("/matcher").getResponseBody.stripPrefix("\"").stripSuffix("\"") shouldBe matcherNode.publicKeyStr
    }

    "owner moves assets/earths to another account and order become an invalid" ignore {
      // todo: reactivate after balance watcher is reimplemented
      // Could not work sometimes because of NODE-546
      "order with assets" - {
        "moved assets, insufficient assets" in {
          val oldestOrderId = bobPlacesAssetOrder(8000, twoAssetsPair, bobNewAsset)
          val newestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)

          val transferId = bobNode.transfer(bobNode.address, aliceNode.address, 3050, TransactionFee, Some(bobNewAsset), None).id
          nodes.waitForHeightAriseAndTxPresent(transferId)

          withClue(s"The oldest order '$oldestOrderId' was cancelled") {
            matcherNode.waitOrderStatus(bobEarthsPair, oldestOrderId, "Cancelled")
          }
          withClue(s"The newest order '$newestOrderId' is still active") {
            matcherNode.waitOrderStatus(bobEarthsPair, newestOrderId, "Accepted")
          }

          // Cleanup
          nodes.waitForHeightArise()
          matcherNode.cancelOrder(bobNode, twoAssetsPair, Some(newestOrderId)).status should be("OrderCanceled")

          val transferBackId = aliceNode.transfer(aliceNode.address, bobNode.address, 3050, TransactionFee, Some(bobNewAsset), None).id
          nodes.waitForHeightAriseAndTxPresent(transferBackId)
        }

        "leased earths, insufficient fee" in {
          val bobBalance    = bobNode.accountBalances(bobNode.address)._1
          val oldestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)
          val newestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)

          // TransactionFee for leasing, matcherFee for one order
          val leaseAmount = bobBalance - TransactionFee - matcherFee
          val leaseId     = bobNode.lease(bobNode.address, aliceNode.address, leaseAmount, TransactionFee).id
          nodes.waitForHeightAriseAndTxPresent(leaseId)

          withClue(s"The oldest order '$oldestOrderId' was cancelled") {
            matcherNode.waitOrderStatus(bobEarthsPair, oldestOrderId, "Cancelled")
          }
          withClue(s"The newest order '$newestOrderId' is still active") {
            matcherNode.waitOrderStatus(bobEarthsPair, newestOrderId, "Accepted")
          }

          // Cleanup
          nodes.waitForHeightArise()
          matcherNode.cancelOrder(bobNode, twoAssetsPair, Some(newestOrderId)).status should be("OrderCanceled")
          val cancelLeaseId = bobNode.cancelLease(bobNode.address, leaseId, TransactionFee).id
          nodes.waitForHeightAriseAndTxPresent(cancelLeaseId)
        }

        "moved earths, insufficient fee" in {
          val bobBalance    = matcherNode.accountBalances(bobNode.address)._1
          val oldestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)
          val newestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)

          // TransactionFee for leasing, matcherFee for one order
          val transferAmount = bobBalance - TransactionFee - matcherFee
          val transferId     = bobNode.transfer(bobNode.address, aliceNode.address, transferAmount, TransactionFee, None, None).id
          nodes.waitForHeightAriseAndTxPresent(transferId)

          withClue(s"The oldest order '$oldestOrderId' was cancelled") {
            matcherNode.waitOrderStatus(bobEarthsPair, oldestOrderId, "Cancelled")
          }
          withClue(s"The newest order '$newestOrderId' is still active") {
            matcherNode.waitOrderStatus(bobEarthsPair, newestOrderId, "Accepted")
          }

          // Cleanup
          nodes.waitForHeightArise()
          matcherNode.cancelOrder(bobNode, twoAssetsPair, Some(newestOrderId)).status should be("OrderCanceled")
          val transferBackId = aliceNode.transfer(aliceNode.address, bobNode.address, transferAmount, TransactionFee, None, None).id
          nodes.waitForHeightAriseAndTxPresent(transferBackId)
        }
      }

      "order with earths" - {
        "leased earths, insufficient fee for one ExchangeTransaction" in {
          // Amount of earths in order is smaller than fee
          val bobBalance = bobNode.accountBalances(bobNode.address)._1

          val oldestOrderId = bobPlacesEarthOrder(bobEarthsPair, 10.earths * Order.PriceConstant, 1)
          val newestOrderId = bobPlacesEarthOrder(bobEarthsPair, 10.earths * Order.PriceConstant, 1)

          //      waitForOrderStatus(matcherNode, bobAssetIdRaw, id, "Accepted")
          val leaseAmount = bobBalance - TransactionFee - 10.earths - matcherFee
          val leaseId     = bobNode.lease(bobNode.address, aliceNode.address, leaseAmount, TransactionFee).id
          nodes.waitForHeightAriseAndTxPresent(leaseId)

          withClue(s"The newest order '$oldestOrderId' is Cancelled") {
            matcherNode.waitOrderStatus(bobEarthsPair, oldestOrderId, "Cancelled")
          }
          withClue(s"The newest order '$newestOrderId' is still active") {
            matcherNode.waitOrderStatus(bobEarthsPair, newestOrderId, "Accepted")
          }

          // Cleanup
          nodes.waitForHeightArise()
          matcherNode.cancelOrder(bobNode, bobEarthsPair, Some(newestOrderId)).status should be("OrderCanceled")
          val cancelLeaseId = bobNode.cancelLease(bobNode.address, leaseId, TransactionFee).id
          nodes.waitForHeightAriseAndTxPresent(cancelLeaseId)
        }

        "leased earths, insufficient earths" in {
          val bobBalance = bobNode.accountBalances(bobNode.address)._1
          val price      = 1.earths
          val order2     = bobPlacesEarthOrder(bobEarthsPair, price * Order.PriceConstant, 1)

          val leaseAmount = bobBalance - TransactionFee - price / 2
          val leaseId     = bobNode.lease(bobNode.address, aliceNode.address, leaseAmount, TransactionFee).id
          nodes.waitForHeightAriseAndTxPresent(leaseId)

          withClue(s"The order '$order2' was cancelled") {
            matcherNode.waitOrderStatus(bobEarthsPair, order2, "Cancelled")
          }

          // Cleanup
          nodes.waitForHeightArise()
          val cancelLeaseId = bobNode.cancelLease(bobNode.address, leaseId, TransactionFee).id
          nodes.waitForHeightAriseAndTxPresent(cancelLeaseId)
        }

        "moved earths, insufficient fee" in {
          // Amount of earths in order is smaller than fee
          val bobBalance = bobNode.accountBalances(bobNode.address)._1
          val price      = TransactionFee / 2
          val order3     = bobPlacesEarthOrder(bobEarthsPair, price * Order.PriceConstant, 1)

          val transferAmount = bobBalance - TransactionFee - price
          val txId           = bobNode.transfer(bobNode.address, aliceNode.address, transferAmount, TransactionFee, None, None).id
          nodes.waitForHeightAriseAndTxPresent(txId)

          withClue(s"The order '$order3' was cancelled") {
            matcherNode.waitOrderStatus(bobEarthsPair, order3, "Cancelled")
          }

          // Cleanup
          nodes.waitForHeightArise()
          val transferBackId = aliceNode.transfer(aliceNode.address, bobNode.address, transferAmount, TransactionFee, None, None).id
          nodes.waitForHeightAriseAndTxPresent(transferBackId)
        }

      }
    }
  }

  def bobPlacesEarthOrder(assetPair: AssetPair, price: Price, amount: Long): String = {
    val bobOrder = matcherNode.prepareOrder(bobNode, assetPair, OrderType.BUY, price, amount)
    val order    = matcherNode.placeOrder(bobOrder).message.id
    matcherNode.waitOrderStatus(assetPair, order, "Accepted")
    order
  }

  def bobPlacesAssetOrder(bobCoinAmount: Int, twoAssetsPair: AssetPair, assetId: String): String = {
    val decodedAsset = ByteStr.decodeBase58(assetId).get
    val bobOrder = if (twoAssetsPair.amountAsset.contains(decodedAsset)) {
      matcherNode.prepareOrder(bobNode, twoAssetsPair, OrderType.SELL, 1 * Order.PriceConstant, bobCoinAmount)
    } else {
      matcherNode.prepareOrder(bobNode, twoAssetsPair, OrderType.BUY, bobCoinAmount * Order.PriceConstant, 1)
    }
    val order = matcherNode.placeOrder(bobOrder)
    matcherNode.waitOrderStatus(twoAssetsPair, order.message.id, "Accepted")
    order.message.id
  }

}
