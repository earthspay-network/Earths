package com.earthspay.it.sync.matcher

import com.typesafe.config.Config
import com.earthspay.it.{Node, ReportingTestName}
import com.earthspay.it.api.SyncHttpApi._
import com.earthspay.it.api.SyncMatcherHttpApi._
import com.earthspay.it.sync.matcher.config.MatcherPriceAssetConfig._
import com.earthspay.it.transactions.NodesFromDocker
import com.earthspay.transaction.assets.exchange.OrderType._
import com.earthspay.transaction.assets.exchange.Order.PriceConstant
import org.scalatest.{BeforeAndAfterAll, FreeSpec, Matchers}

class OrderBookTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with NodesFromDocker with ReportingTestName {

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head
  private def aliceNode   = nodes(1)
  private def bobNode     = nodes(2)

  matcherNode.signedIssue(createSignedIssueRequest(IssueUsdTx))
  matcherNode.signedIssue(createSignedIssueRequest(IssueWctTx))
  nodes.waitForHeightArise()

  case class ReservedBalances(wct: Long, usd: Long, earths: Long)
  def reservedBalancesOf(node: Node): ReservedBalances = {
    val reservedBalances = matcherNode.reservedBalance(node)
    ReservedBalances(
      reservedBalances.getOrElse(WctId.toString, 0),
      reservedBalances.getOrElse(UsdId.toString, 0),
      reservedBalances.getOrElse("EARTHS", 0)
    )
  }

  val (amount, price) = (1000L, PriceConstant)

  "When delete order book" - {
    val buyOrder        = matcherNode.placeOrder(aliceNode, wctUsdPair, BUY, 2 * amount, price).message.id
    val anotherBuyOrder = matcherNode.placeOrder(aliceNode, wctUsdPair, BUY, amount, price).message.id

    val submitted = matcherNode.placeOrder(bobNode, wctUsdPair, SELL, amount, price).message.id

    val sellOrder = matcherNode.placeOrder(bobNode, wctUsdPair, SELL, amount, 2 * price).message.id

    matcherNode.waitOrderStatus(wctUsdPair, buyOrder, "PartiallyFilled")
    matcherNode.waitOrderStatus(wctUsdPair, submitted, "Filled")

    val (aliceRBForOnePair, bobRBForOnePair) = (reservedBalancesOf(aliceNode), reservedBalancesOf(bobNode))

    val buyOrderForAnotherPair  = matcherNode.placeOrder(aliceNode, wctEarthsPair, BUY, amount, price).message.id
    val sellOrderForAnotherPair = matcherNode.placeOrder(bobNode, wctEarthsPair, SELL, amount, 2 * price).message.id

    matcherNode.waitOrderStatus(wctEarthsPair, buyOrderForAnotherPair, "Accepted")
    matcherNode.waitOrderStatus(wctEarthsPair, sellOrderForAnotherPair, "Accepted")

    val (aliceRBForBothPairs, bobRBForBothPairs) = (reservedBalancesOf(aliceNode), reservedBalancesOf(bobNode))

    val marketStatusBeforeDeletion = matcherNode.marketStatus(wctUsdPair)

    matcherNode.deleteOrderBook(wctUsdPair)

    "orders by the pair should be canceled" in {
      matcherNode.waitOrderStatus(wctUsdPair, buyOrder, "Cancelled")
      matcherNode.waitOrderStatus(wctUsdPair, anotherBuyOrder, "Cancelled")
      matcherNode.waitOrderStatus(wctUsdPair, sellOrder, "Cancelled")
    }

    "orderbook was really deleted" in {
      val orderBook = matcherNode.orderBook(wctUsdPair)
      orderBook.bids shouldBe empty
      orderBook.asks shouldBe empty
    }

    "reserved balances should be released for the pair" in {
      val (aliceReservedBalances, bobReservedBalances) = (reservedBalancesOf(aliceNode), reservedBalancesOf(bobNode))
      aliceReservedBalances.usd shouldBe 0
      aliceReservedBalances.earths shouldBe (aliceRBForBothPairs.earths - aliceRBForOnePair.earths)
      bobReservedBalances.wct shouldBe (bobRBForBothPairs.wct - bobRBForOnePair.wct)
      bobReservedBalances.earths shouldBe (bobRBForBothPairs.earths - bobRBForOnePair.earths)
    }

    "it should not affect other pairs and their orders" in {
      matcherNode.orderStatus(buyOrderForAnotherPair, wctEarthsPair).status shouldBe "Accepted"
      matcherNode.orderStatus(sellOrderForAnotherPair, wctEarthsPair).status shouldBe "Accepted"

      val orderBook = matcherNode.orderBook(wctEarthsPair)
      orderBook.bids shouldNot be(empty)
      orderBook.asks shouldNot be(empty)
    }

    "it should not affect market status" in {
      matcherNode.marketStatus(wctUsdPair) shouldEqual marketStatusBeforeDeletion
    }
  }

}
