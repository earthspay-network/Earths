package com.earthspay.it.sync.matcher

import com.typesafe.config.Config
import com.earthspay.it.ReportingTestName
import com.earthspay.it.api.SyncHttpApi._
import com.earthspay.it.api.SyncMatcherHttpApi._
import com.earthspay.it.sync.matcher.config.MatcherPriceAssetConfig._
import com.earthspay.it.transactions.NodesFromDocker
import com.earthspay.it.util._
import com.earthspay.transaction.assets.exchange.{AssetPair, OrderType}
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.duration._

class CancelOrderTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with CancelAfterFailure with NodesFromDocker with ReportingTestName {

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head

  private def bobNode = nodes(2)

  private val earthsBtcPair = AssetPair(None, Some(BtcId))

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    matcherNode.signedIssue(createSignedIssueRequest(IssueUsdTx))
    matcherNode.signedIssue(createSignedIssueRequest(IssueBtcTx))
    nodes.waitForHeightArise()
  }

  "Order can be canceled" - {
    "by sender" in {
      val orderId = matcherNode.placeOrder(bobNode, earthsUsdPair, OrderType.SELL, 100.earths, 800).message.id
      matcherNode.waitOrderStatus(earthsUsdPair, orderId, "Accepted", 1.minute)
      matcherNode.cancelOrder(bobNode, earthsUsdPair, orderId)
      matcherNode.waitOrderStatus(earthsUsdPair, orderId, "Cancelled", 1.minute)
      matcherNode.orderHistoryByPair(bobNode, earthsUsdPair).collectFirst {
        case o if o.id == orderId => o.status shouldEqual "Cancelled"
      }
    }
    "with API key" in {
      val orderId = matcherNode.placeOrder(bobNode, earthsUsdPair, OrderType.SELL, 100.earths, 800).message.id
      matcherNode.waitOrderStatus(earthsUsdPair, orderId, "Accepted", 1.minute)

      matcherNode.cancelOrderWithApiKey(orderId)
      matcherNode.waitOrderStatus(earthsUsdPair, orderId, "Cancelled", 1.minute)

      matcherNode.fullOrderHistory(bobNode).filter(_.id == orderId).head.status shouldBe "Cancelled"
      matcherNode.orderHistoryByPair(bobNode, earthsUsdPair).filter(_.id == orderId).head.status shouldBe "Cancelled"

      val orderBook = matcherNode.orderBook(earthsUsdPair)
      orderBook.bids shouldBe empty
      orderBook.asks shouldBe empty
    }
  }

  "Cancel is rejected" - {
    "when request sender is not the sender of and order" in {
      val orderId = matcherNode.placeOrder(bobNode, earthsUsdPair, OrderType.SELL, 100.earths, 800).message.id
      matcherNode.waitOrderStatus(earthsUsdPair, orderId, "Accepted", 1.minute)

      matcherNode.expectCancelRejected(matcherNode.privateKey, earthsUsdPair, orderId)
    }
  }

  "Batch cancel" - {
    "works for" - {
      "all orders placed by an address" in {
        val usdOrderIds = 1 to 5 map { i =>
          matcherNode.placeOrder(bobNode, earthsUsdPair, OrderType.SELL, 100.earths + i, 400).message.id
        }

        val btcOrderIds = 1 to 5 map { i =>
          matcherNode.placeOrder(bobNode, earthsBtcPair, OrderType.BUY, 100.earths + i, 400).message.id
        }

        (usdOrderIds ++ btcOrderIds).foreach(id => matcherNode.waitOrderStatus(earthsUsdPair, id, "Accepted"))

        matcherNode.cancelAllOrders(bobNode)

        (usdOrderIds ++ btcOrderIds).foreach(id => matcherNode.waitOrderStatus(earthsUsdPair, id, "Cancelled"))
      }
      "a pair" in {
        val usdOrderIds = 1 to 5 map { i =>
          matcherNode.placeOrder(bobNode, earthsUsdPair, OrderType.SELL, 100.earths + i, 400).message.id
        }

        val btcOrderIds = 1 to 5 map { i =>
          matcherNode.placeOrder(bobNode, earthsBtcPair, OrderType.BUY, 100.earths + i, 400).message.id
        }

        (usdOrderIds ++ btcOrderIds).foreach(id => matcherNode.waitOrderStatus(earthsUsdPair, id, "Accepted"))

        matcherNode.cancelOrdersForPair(bobNode, earthsBtcPair)

        btcOrderIds.foreach(id => matcherNode.waitOrderStatus(earthsUsdPair, id, "Cancelled"))
        usdOrderIds.foreach(id => matcherNode.waitOrderStatus(earthsUsdPair, id, "Accepted"))
      }
    }
  }
}
