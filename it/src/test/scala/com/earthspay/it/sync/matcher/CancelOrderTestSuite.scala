package com.earthspay.it.sync.matcher

import com.typesafe.config.{Config, ConfigFactory}
import com.earthspay.account.PrivateKeyAccount
import com.earthspay.api.http.assets.SignedIssueV1Request
import com.earthspay.it.ReportingTestName
import com.earthspay.it.api.SyncHttpApi._
import com.earthspay.it.api.SyncMatcherHttpApi._
import com.earthspay.it.sync.CustomFeeTransactionSuite.defaultAssetQuantity
import com.earthspay.it.transactions.NodesFromDocker
import com.earthspay.it.util._
import com.earthspay.transaction.AssetId
import com.earthspay.transaction.assets.IssueTransactionV1
import com.earthspay.transaction.assets.exchange.OrderType.BUY
import com.earthspay.transaction.assets.exchange.{AssetPair, Order, OrderType}
import com.earthspay.utils.Base58
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.duration._
import scala.math.BigDecimal.RoundingMode
import scala.util.{Random, Try}
import com.earthspay.it.sync.matcher.config.MatcherPriceAssetConfig._

class CancelOrderTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with CancelAfterFailure with NodesFromDocker with ReportingTestName {

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head

  private def aliceNode = nodes(1)

  private def bobNode = nodes(2)

  matcherNode.signedIssue(createSignedIssueRequest(IssueUsdTx))
  matcherNode.signedIssue(createSignedIssueRequest(IssueWctTx))
  nodes.waitForHeightArise()

  "cancel order using api-key" in {
    val orderId = matcherNode.placeOrder(bobNode, earthsUsdPair, OrderType.SELL, 800, 100.earths).message.id
    matcherNode.waitOrderStatus(earthsUsdPair, orderId, "Accepted", 1.minute)

    matcherNode.cancelOrderWithApiKey(orderId)
    matcherNode.waitOrderStatus(earthsUsdPair, orderId, "Cancelled", 1.minute)

    matcherNode.fullOrderHistory(bobNode).filter(_.id == orderId).head.status shouldBe "Cancelled"
    matcherNode.orderHistoryByPair(bobNode, earthsUsdPair).filter(_.id == orderId).head.status shouldBe "Cancelled"
    matcherNode.orderBook(earthsUsdPair).bids shouldBe empty
    matcherNode.orderBook(earthsUsdPair).asks shouldBe empty

    matcherNode.deleteOrder(bobNode, earthsUsdPair, Some(orderId))
    matcherNode.orderStatus(orderId, earthsUsdPair, false).status shouldBe "NotFound"

  }

  "Alice and Bob trade EARTHS-USD" - {
    "place usd-earths order" in {
      // Alice wants to sell USD for Earths
      val orderId1      = matcherNode.placeOrder(bobNode, earthsUsdPair, OrderType.SELL, 800, 100.earths).message.id
      val orderId2      = matcherNode.placeOrder(bobNode, earthsUsdPair, OrderType.SELL, 700, 100.earths).message.id
      val bobSellOrder3 = matcherNode.placeOrder(bobNode, earthsUsdPair, OrderType.SELL, 600, 100.earths).message.id

      matcherNode.fullOrderHistory(aliceNode)
      matcherNode.fullOrderHistory(bobNode)

      matcherNode.waitOrderStatus(earthsUsdPair, bobSellOrder3, "Accepted", 1.minute)

      val aliceOrder = matcherNode.prepareOrder(aliceNode, earthsUsdPair, OrderType.BUY, 800, 0.00125.earths)
      matcherNode.placeOrder(aliceOrder).message.id

      Thread.sleep(2000)
      matcherNode.fullOrderHistory(aliceNode)
      val orders = matcherNode.fullOrderHistory(bobNode)
      for (orderId <- Seq(orderId1, orderId2)) {
        orders.filter(_.id == orderId).head.status shouldBe "Accepted"
      }
    }

  }

  def correctAmount(a: Long, price: Long): Long = {
    val min = (BigDecimal(Order.PriceConstant) / price).setScale(0, RoundingMode.CEILING)
    if (min > 0)
      Try(((BigDecimal(a) / min).toBigInt() * min.toBigInt()).bigInteger.longValueExact()).getOrElse(Long.MaxValue)
    else
      a
  }

  def receiveAmount(ot: OrderType, matchPrice: Long, matchAmount: Long): Long =
    if (ot == BUY) correctAmount(matchAmount, matchPrice)
    else {
      (BigInt(matchAmount) * matchPrice / Order.PriceConstant).bigInteger.longValueExact()
    }

}
