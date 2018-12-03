package com.earthspay.it.sync.matcher

import com.typesafe.config.Config
import com.earthspay.it._
import com.earthspay.it.api.SyncHttpApi._
import com.earthspay.it.api.SyncMatcherHttpApi._
import com.earthspay.it.sync._
import com.earthspay.it.sync.matcher.config.MatcherDefaultConfig._
import com.earthspay.it.transactions.NodesFromDocker
import com.earthspay.state.ByteStr
import com.earthspay.transaction.assets.exchange.{AssetPair, Order, OrderType}
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.duration._
import scala.util.Random

class MatcherMassOrdersTestSuite
    extends FreeSpec
    with NodesFromDocker
    with ReportingTestName
    with Matchers
    with BeforeAndAfterAll
    with CancelAfterFailure {

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head
  private def aliceNode   = nodes(1)
  private def bobNode     = nodes(2)

  "Create orders with statuses FILL, PARTIAL, CANCELLED, ACTIVE" - {

    // Alice issues new assets
    val aliceAsset =
      aliceNode.issue(aliceNode.address, "AliceCoin", "AliceCoin for matcher's tests", someAssetAmount, 0, reissuable = false, 100000000L).id
    nodes.waitForHeightAriseAndTxPresent(aliceAsset)

    val aliceSecondAsset = aliceNode
      .issue(aliceNode.address, "AliceSecondCoin", "AliceSecondCoin for matcher's tests", someAssetAmount, 0, reissuable = false, 100000000L)
      .id
    nodes.waitForHeightAriseAndTxPresent(aliceSecondAsset)

    val aliceEarthsPair       = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)
    val aliceSecondEarthsPair = AssetPair(ByteStr.decodeBase58(aliceSecondAsset).toOption, None)

    // Check balances on Alice's account
    aliceNode.assertAssetBalance(aliceNode.address, aliceAsset, someAssetAmount)
    aliceNode.assertAssetBalance(aliceNode.address, aliceSecondAsset, someAssetAmount)
    matcherNode.assertAssetBalance(matcherNode.address, aliceAsset, 0)

    val transfer1ToBobId = aliceNode.transfer(aliceNode.address, bobNode.address, someAssetAmount / 2, 100000, Some(aliceAsset), None).id
    nodes.waitForHeightAriseAndTxPresent(transfer1ToBobId)

    val transfer2ToBobId = aliceNode.transfer(aliceNode.address, bobNode.address, someAssetAmount / 2, 100000, Some(aliceSecondAsset), None).id
    nodes.waitForHeightAriseAndTxPresent(transfer2ToBobId)

    bobNode.assertAssetBalance(bobNode.address, aliceAsset, someAssetAmount / 2)
    bobNode.assertAssetBalance(bobNode.address, aliceSecondAsset, someAssetAmount / 2)

    // Alice places sell orders
    val aliceOrderIdFill = matcherNode
      .placeOrder(aliceNode, aliceSecondEarthsPair, OrderType.SELL, 3, Order.PriceConstant, 10.minutes)
      .message
      .id

    val alicePartialOrderId = matcherNode
      .placeOrder(aliceNode, aliceSecondEarthsPair, OrderType.SELL, 3, Order.PriceConstant, 10.minutes)
      .message
      .id

    val aliceOrderToCancelId =
      matcherNode
        .placeOrder(aliceNode, aliceSecondEarthsPair, OrderType.SELL, 3, Order.PriceConstant, 70.seconds)
        .message
        .id

    val aliceActiveOrderId = matcherNode
      .placeOrder(aliceNode, aliceSecondEarthsPair, OrderType.SELL, 3, Order.PriceConstant + 100000000, 10.minutes)
      .message
      .id

    matcherNode.waitOrderStatus(aliceSecondEarthsPair, aliceOrderToCancelId, "Cancelled", 2.minutes)

    //Bob orders should partially fill one Alice order and fill another
    ordersRequestsGen(2, bobNode, aliceSecondEarthsPair, OrderType.BUY, 2)

    //check orders after filling
    matcherNode.waitOrderStatus(aliceSecondEarthsPair, alicePartialOrderId, "PartiallyFilled")

    orderStatus(aliceNode, aliceSecondEarthsPair, aliceOrderIdFill, "Filled")
    orderStatus(aliceNode, aliceSecondEarthsPair, alicePartialOrderId, "PartiallyFilled")

    "Mass orders creation with random lifetime. Active orders still in list" in {
      matcherNode.ordersByAddress(aliceNode, activeOnly = false).length shouldBe 4
      matcherNode.ordersByAddress(aliceNode, activeOnly = true).length shouldBe 2

      matcherNode.ordersByAddress(bobNode, activeOnly = false).length shouldBe 2
      matcherNode.ordersByAddress(bobNode, activeOnly = true).length shouldBe 0

      val orderIds = matcherNode.fullOrderHistory(aliceNode).map(_.id)

      orderIds should contain(aliceActiveOrderId)

      ordersRequestsGen(orderLimit + 1, aliceNode, aliceEarthsPair, OrderType.SELL, 3)

      //wait for some orders cancelled
      Thread.sleep(5000)
      val bobsOrderIds = ordersRequestsGen(orderLimit + 1, bobNode, aliceEarthsPair, OrderType.BUY, 2)
      Thread.sleep(5000)

      // Alice check that order Active order is still in list
      val orderIdsAfterMatching = matcherNode.fullOrderHistory(aliceNode).map(_.id)

      orderIdsAfterMatching should contain(aliceActiveOrderId)
      orderIdsAfterMatching should contain(alicePartialOrderId)

      matcherNode.waitOrderStatus(aliceSecondEarthsPair, aliceActiveOrderId, "Accepted")
      matcherNode.waitOrderStatus(aliceSecondEarthsPair, alicePartialOrderId, "PartiallyFilled")

      matcherNode.fullOrderHistory(bobNode).map(_.id) should equal(bobsOrderIds.drop(1).reverse)
      matcherNode.orderHistoryByPair(bobNode, aliceEarthsPair).map(_.id) should equal(bobsOrderIds.drop(1).reverse)
    }

    "Filled and Cancelled orders should be after Partial And Accepted" in {
      val lastIdxOfActiveOrder =
        matcherNode.fullOrderHistory(aliceNode).lastIndexWhere(o => o.status.equals("Accepted") || o.status.equals("PartiallyFilled"))
      val firstIdxOfClosedOrder = matcherNode.fullOrderHistory(aliceNode).indexWhere(o => o.status.equals("Filled") || o.status.equals("Cancelled"))
      lastIdxOfActiveOrder should be < firstIdxOfClosedOrder
    }

    "Accepted and PartiallyFilled orders should be sorted by timestamp." in {
      val activeAndPartialOrders =
        matcherNode.fullOrderHistory(aliceNode).filter(o => o.status.equals("Accepted") || o.status.equals("PartiallyFilled")).map(_.timestamp)
      activeAndPartialOrders.reverse shouldBe sorted
    }

    "Filled and Cancelled orders should be sorted by timestamp." in {
      val filledAndCancelledOrders =
        matcherNode.fullOrderHistory(aliceNode).filter(o => o.status.equals("Filled") || o.status.equals("Cancelled")).map(_.timestamp)
      filledAndCancelledOrders.reverse shouldBe sorted
    }

    "check order history orders count after fill" in {
      val aliceOrderHistory = matcherNode.fullOrderHistory(aliceNode)
      aliceOrderHistory.size shouldBe orderLimit
      val aliceOrderHistoryByPair = matcherNode.orderHistoryByPair(aliceNode, aliceEarthsPair)
      aliceOrderHistoryByPair.size shouldBe orderLimit
    }

  }

  private def ordersRequestsGen(n: Int, node: Node, assetPair: AssetPair, orderType: OrderType, amount: Long): Seq[String] = {
    val orderIds = 1 to n map (_ => {
      matcherNode
        .placeOrder(node, assetPair, orderType, amount, Order.PriceConstant, (120 + Random.nextInt(70)).seconds)
        .message
        .id
    })
    orderIds
  }

  private def orderStatus(node: Node, assetPair: AssetPair, orderId: String, expectedStatus: String) = {
    matcherNode.waitOrderStatus(assetPair, orderId, expectedStatus)
    matcherNode.fullOrderHistory(node).filter(_.id == orderId).seq.head.status shouldBe expectedStatus
  }
}
