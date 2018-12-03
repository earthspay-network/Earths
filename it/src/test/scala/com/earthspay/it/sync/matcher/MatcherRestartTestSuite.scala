package com.earthspay.it.sync.matcher

import com.typesafe.config.{Config, ConfigFactory}
import com.earthspay.it.api.SyncHttpApi._
import com.earthspay.it.api.SyncMatcherHttpApi._
import com.earthspay.it.transactions.NodesFromDocker
import com.earthspay.it.util._
import com.earthspay.it.{TransferSending, _}
import com.earthspay.state.ByteStr
import com.earthspay.transaction.assets.exchange.{AssetPair, Order, OrderType}
import org.scalatest.concurrent.Eventually
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.duration._
import scala.util.Random
import com.earthspay.it.sync._
import com.earthspay.it.sync.matcher.config.MatcherDefaultConfig._

class MatcherRestartTestSuite
    extends FreeSpec
    with Matchers
    with BeforeAndAfterAll
    with CancelAfterFailure
    with ReportingTestName
    with NodesFromDocker
    with TransferSending
    with Eventually {

  override protected def nodeConfigs: Seq[Config] = Configs
  private def matcherNode                         = nodes.head
  private def aliceNode                           = nodes(1)

  "check order execution" - {
    // Alice issues new asset
    val aliceAsset =
      aliceNode.issue(aliceNode.address, "DisconnectCoin", "Alice's coin for disconnect tests", someAssetAmount, 0, reissuable = false, 100000000L).id
    nodes.waitForHeightAriseAndTxPresent(aliceAsset)

    val aliceEarthsPair = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)
    // check assets's balances
    aliceNode.assertAssetBalance(aliceNode.address, aliceAsset, someAssetAmount)
    aliceNode.assertAssetBalance(matcherNode.address, aliceAsset, 0)

    "make order and after matcher's restart try to cancel it" in {
      // Alice places sell order
      val aliceOrder = matcherNode
        .placeOrder(aliceNode, aliceEarthsPair, OrderType.SELL, 2.earths * Order.PriceConstant, 500)
      aliceOrder.status shouldBe "OrderAccepted"
      val firstOrder = aliceOrder.message.id

      matcherNode.waitOrderStatus(aliceEarthsPair, firstOrder, "Accepted")

      // check that order is correct
      eventually {
        val orders = matcherNode.orderBook(aliceEarthsPair)
        orders.asks.head.amount shouldBe 500
        orders.asks.head.price shouldBe 2.earths * Order.PriceConstant
      }

      // sell order should be in the aliceNode orderbook
      matcherNode.fullOrderHistory(aliceNode).head.status shouldBe "Accepted"

      // reboot matcher's node
      docker.killAndStartContainer(dockerNodes().head)
      Thread.sleep(60.seconds.toMillis)

      val height = nodes.map(_.height).max

      matcherNode.waitForHeight(height + 1, 40.seconds)
      matcherNode.waitOrderStatus(aliceEarthsPair, firstOrder, "Accepted")
      matcherNode.fullOrderHistory(aliceNode).head.status shouldBe "Accepted"

      eventually {
        val orders1 = matcherNode.orderBook(aliceEarthsPair)
        orders1.asks.head.amount shouldBe 500
        orders1.asks.head.price shouldBe 2.earths * Order.PriceConstant
      }

      val aliceSecondOrder = matcherNode.placeOrder(aliceNode, aliceEarthsPair, OrderType.SELL, 2.earths * Order.PriceConstant, 500, 5.minutes)
      aliceSecondOrder.status shouldBe "OrderAccepted"

      eventually {
        val orders2 = matcherNode.orderBook(aliceEarthsPair)
        orders2.asks.head.amount shouldBe 1000
        orders2.asks.head.price shouldBe 2.earths * Order.PriceConstant
      }

      val cancel = matcherNode.cancelOrder(aliceNode, aliceEarthsPair, Some(firstOrder))
      cancel.status should be("OrderCanceled")

      eventually {
        val orders3 = matcherNode.orderBook(aliceEarthsPair)
        orders3.asks.head.amount shouldBe 500
      }

      matcherNode.waitOrderStatus(aliceEarthsPair, firstOrder, "Cancelled")
      matcherNode.fullOrderHistory(aliceNode).head.status shouldBe "Accepted"
    }
  }
}
