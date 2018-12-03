package com.earthspay.it.sync.matcher

import com.typesafe.config.Config
import com.earthspay.it.ReportingTestName
import com.earthspay.it.api.SyncHttpApi._
import com.earthspay.it.api.SyncMatcherHttpApi._
import com.earthspay.it.api.{AssetDecimalsInfo, LevelResponse}
import com.earthspay.it.sync._
import com.earthspay.it.sync.matcher.config.MatcherDefaultConfig._
import com.earthspay.it.transactions.NodesFromDocker
import com.earthspay.it.util._
import com.earthspay.state.ByteStr
import com.earthspay.transaction.assets.exchange.AssetPair
import com.earthspay.transaction.assets.exchange.Order._
import com.earthspay.transaction.assets.exchange.OrderType._
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.duration._

class MatcherTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with CancelAfterFailure with NodesFromDocker with ReportingTestName {

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head

  private def aliceNode = nodes(1)

  private def bobNode = nodes(2)

  private val aliceSellAmount         = 500
  private val TransactionFee          = 300000
  private val amountAssetName         = "AliceCoin"
  private val AssetQuantity           = 1000
  private val aliceCoinDecimals: Byte = 0

  "Check cross ordering between Alice and Bob " - {
    // Alice issues new asset
    val aliceAsset = aliceNode
      .issue(aliceNode.address, amountAssetName, "AliceCoin for matcher's tests", AssetQuantity, aliceCoinDecimals, reissuable = false, 100000000L)
      .id
    nodes.waitForHeightAriseAndTxPresent(aliceAsset)

    val aliceEarthsPair = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)

    val order1         = matcherNode.prepareOrder(aliceNode, aliceEarthsPair, SELL, aliceSellAmount, 2000.earths)
    val order1Response = matcherNode.placeOrder(order1)

    "can't place an order with the same timestamp" in {
      val order2 = sign(order1.copy(amount = order1.amount + 1), aliceNode.privateKey)
      matcherNode.expectIncorrectOrderPlacement(order2, 400, "OrderRejected") shouldBe true
    }

    "assert addresses balances" in {
      aliceNode.assertAssetBalance(aliceNode.address, aliceAsset, AssetQuantity)
      matcherNode.assertAssetBalance(matcherNode.address, aliceAsset, 0)
      bobNode.assertAssetBalance(bobNode.address, aliceAsset, 0)
    }

    "matcher should respond with Public key" in {
      matcherNode.matcherGet("/matcher").getResponseBody.stripPrefix("\"").stripSuffix("\"") shouldBe matcherNode.publicKeyStr
    }

    "get opened trading markets" in {
      val openMarkets = matcherNode.tradingMarkets()
      openMarkets.markets.size shouldBe 1
      val markets = openMarkets.markets.head

      markets.amountAssetName shouldBe amountAssetName
      markets.amountAssetInfo shouldBe Some(AssetDecimalsInfo(aliceCoinDecimals))

      markets.priceAssetName shouldBe "EARTHS"
      markets.priceAssetInfo shouldBe Some(AssetDecimalsInfo(8))
    }

    "sell order could be placed correctly" - {
      "alice places sell order" in {
        order1Response.status shouldBe "OrderAccepted"

        // Alice checks that the order in order book
        matcherNode.waitOrderStatus(aliceEarthsPair, order1Response.message.id, "Accepted")

        // Alice check that order is correct
        val orders = matcherNode.orderBook(aliceEarthsPair)
        orders.asks.head.amount shouldBe aliceSellAmount
        orders.asks.head.price shouldBe 2000.earths
      }

      "frozen amount should be listed via matcherBalance REST endpoint" in {
        matcherNode.reservedBalance(aliceNode) shouldBe Map(aliceAsset -> aliceSellAmount)

        matcherNode.reservedBalance(bobNode) shouldBe Map()
      }

      "and should be listed by trader's publiс key via REST" in {
        matcherNode.fullOrderHistory(aliceNode).map(_.id) should contain(order1Response.message.id)
      }

      "and should match with buy order" in {
        val bobBalance     = bobNode.accountBalances(bobNode.address)._1
        val matcherBalance = matcherNode.accountBalances(matcherNode.address)._1
        val aliceBalance   = aliceNode.accountBalances(aliceNode.address)._1

        // Bob places a buy order
        val order2 = matcherNode.placeOrder(bobNode, aliceEarthsPair, BUY, 200, 2000.earths)
        order2.status shouldBe "OrderAccepted"

        matcherNode.waitOrderStatus(aliceEarthsPair, order1Response.message.id, "PartiallyFilled")
        matcherNode.waitOrderStatus(aliceEarthsPair, order2.message.id, "Filled")

        matcherNode.orderHistoryByPair(bobNode, aliceEarthsPair).map(_.id) should contain(order2.message.id)
        matcherNode.fullOrderHistory(bobNode).map(_.id) should contain(order2.message.id)

        nodes.waitForHeightArise()

        // Bob checks that asset on his balance
        bobNode.assertAssetBalance(bobNode.address, aliceAsset, 200)

        // Alice checks that part of her order still in the order book
        val orders = matcherNode.orderBook(aliceEarthsPair)
        orders.asks.head.amount shouldBe 300
        orders.asks.head.price shouldBe 2000.earths

        // Alice checks that she sold some assets
        aliceNode.assertAssetBalance(aliceNode.address, aliceAsset, 800)

        // Bob checks that he spent some Earths
        val updatedBobBalance = bobNode.accountBalances(bobNode.address)._1
        updatedBobBalance shouldBe (bobBalance - 2000 * 200 - matcherFee)

        // Alice checks that she received some Earths
        val updatedAliceBalance = aliceNode.accountBalances(aliceNode.address)._1
        updatedAliceBalance shouldBe (aliceBalance + 2000 * 200 - (matcherFee * 200.0 / 500.0).toLong)

        // Matcher checks that it earn fees
        val updatedMatcherBalance = matcherNode.accountBalances(matcherNode.address)._1
        updatedMatcherBalance shouldBe (matcherBalance + matcherFee + (matcherFee * 200.0 / 500.0).toLong - TransactionFee)
      }

      "request activeOnly orders" in {
        val aliceOrders = matcherNode.activeOrderHistory(aliceNode)
        aliceOrders.map(_.id) shouldBe Seq(order1Response.message.id)
        val bobOrders = matcherNode.activeOrderHistory(bobNode)
        bobOrders.map(_.id) shouldBe Seq()
      }

      "submitting sell orders should check availability of asset" in {
        // Bob trying to place order on more assets than he has - order rejected
        val badOrder = matcherNode.prepareOrder(bobNode, aliceEarthsPair, SELL, 300, 1900.earths)
        matcherNode.expectIncorrectOrderPlacement(badOrder, 400, "OrderRejected") should be(true)

        // Bob places order on available amount of assets - order accepted
        val order3 = matcherNode.placeOrder(bobNode, aliceEarthsPair, SELL, 150, 1900.earths)
        order3.status should be("OrderAccepted")

        // Bob checks that the order in the order book
        val orders = matcherNode.orderBook(aliceEarthsPair)
        orders.asks should contain(LevelResponse(150, 1900.earths))
      }

      "buy order should match on few price levels" in {
        val matcherBalance = matcherNode.accountBalances(matcherNode.address)._1
        val aliceBalance   = aliceNode.accountBalances(aliceNode.address)._1
        val bobBalance     = bobNode.accountBalances(bobNode.address)._1

        // Alice places a buy order
        val order4 = matcherNode.placeOrder(aliceNode, aliceEarthsPair, BUY, 350, 2100.earths)
        order4.status should be("OrderAccepted")

        // Where were 2 sells that should fulfill placed order
        matcherNode.waitOrderStatus(aliceEarthsPair, order4.message.id, "Filled")

        // Check balances
        nodes.waitForHeightArise()
        aliceNode.assertAssetBalance(aliceNode.address, aliceAsset, 950)
        bobNode.assertAssetBalance(bobNode.address, aliceAsset, 50)

        val updatedMatcherBalance = matcherNode.accountBalances(matcherNode.address)._1
        updatedMatcherBalance should be(
          matcherBalance - 2 * TransactionFee + matcherFee + (matcherFee * 150.0 / 350.0).toLong + (matcherFee * 200.0 / 350.0).toLong + (matcherFee * 200.0 / 500.0).toLong)

        val updatedBobBalance = bobNode.accountBalances(bobNode.address)._1
        updatedBobBalance should be(bobBalance - matcherFee + 150 * 1900)

        val updatedAliceBalance = aliceNode.accountBalances(aliceNode.address)._1
        updatedAliceBalance should be(
          aliceBalance - (matcherFee * 200.0 / 350.0).toLong - (matcherFee * 150.0 / 350.0).toLong - (matcherFee * 200.0 / 500.0).toLong - 1900 * 150)
      }

      "order could be canceled and resubmitted again" in {
        // Alice cancels the very first order (100 left)
        val status1 = matcherNode.cancelOrder(aliceNode, aliceEarthsPair, order1Response.message.id)
        status1.status should be("OrderCanceled")

        // Alice checks that the order book is empty
        val orders1 = matcherNode.orderBook(aliceEarthsPair)
        orders1.asks.size should be(0)
        orders1.bids.size should be(0)

        // Alice places a new sell order on 100
        val order4 = matcherNode.placeOrder(aliceNode, aliceEarthsPair, SELL, 100, 2000.earths)
        order4.status should be("OrderAccepted")

        // Alice checks that the order is in the order book
        val orders2 = matcherNode.orderBook(aliceEarthsPair)
        orders2.asks should contain(LevelResponse(100, 2000.earths))
      }

      "buy order should execute all open orders and put remaining in order book" in {
        val matcherBalance = matcherNode.accountBalances(matcherNode.address)._1
        val aliceBalance   = aliceNode.accountBalances(aliceNode.address)._1
        val bobBalance     = bobNode.accountBalances(bobNode.address)._1

        // Bob places buy order on amount bigger then left in sell orders
        val order5 = matcherNode.placeOrder(bobNode, aliceEarthsPair, BUY, 130, 2000.earths)
        order5.status should be("OrderAccepted")

        // Check that the order is partially filled
        matcherNode.waitOrderStatus(aliceEarthsPair, order5.message.id, "PartiallyFilled")

        // Check that remaining part of the order is in the order book
        val orders = matcherNode.orderBook(aliceEarthsPair)
        orders.bids should contain(LevelResponse(30, 2000.earths))

        // Check balances
        nodes.waitForHeightArise()
        aliceNode.assertAssetBalance(aliceNode.address, aliceAsset, 850)
        bobNode.assertAssetBalance(bobNode.address, aliceAsset, 150)

        val updatedMatcherBalance = matcherNode.accountBalances(matcherNode.address)._1
        updatedMatcherBalance should be(matcherBalance - TransactionFee + matcherFee + (matcherFee * 100.0 / 130.0).toLong)

        val updatedBobBalance = bobNode.accountBalances(bobNode.address)._1
        updatedBobBalance should be(bobBalance - (matcherFee * 100.0 / 130.0).toLong - 100 * 2000)

        val updatedAliceBalance = aliceNode.accountBalances(aliceNode.address)._1
        updatedAliceBalance should be(aliceBalance - matcherFee + 2000 * 100)
      }

      "market status" in {
        val resp = matcherNode.marketStatus(aliceEarthsPair)

        resp.lastPrice shouldBe Some(2000.earths)
        resp.lastSide shouldBe Some("buy") // Same type as order5
        resp.bid shouldBe Some(2000.earths)
        resp.bidAmount shouldBe Some(30)
        resp.ask shouldBe None
        resp.askAmount shouldBe None
      }

      "request order book for blacklisted pair" in {
        val f = matcherNode.matcherGetStatusCode(s"/matcher/orderbook/$ForbiddenAssetId/EARTHS", 404)
        f.message shouldBe s"Invalid Asset ID: $ForbiddenAssetId"
      }

      "should consider UTX pool when checking the balance" in {
        // Bob issues new asset
        val bobAssetQuantity = 10000
        val bobAssetName     = "BobCoin"
        val bobAsset         = bobNode.issue(bobNode.address, bobAssetName, "Bob's asset", bobAssetQuantity, 0, false, 100000000L).id
        nodes.waitForHeightAriseAndTxPresent(bobAsset)

        aliceNode.assertAssetBalance(aliceNode.address, bobAsset, 0)
        matcherNode.assertAssetBalance(matcherNode.address, bobAsset, 0)
        bobNode.assertAssetBalance(bobNode.address, bobAsset, bobAssetQuantity)
        val bobEarthsPair = AssetPair(ByteStr.decodeBase58(bobAsset).toOption, None)

        def bobOrder = matcherNode.prepareOrder(bobNode, bobEarthsPair, SELL, bobAssetQuantity, 1.earths)

        val order6 = matcherNode.placeOrder(bobOrder)
        matcherNode.waitOrderStatus(bobEarthsPair, order6.message.id, "Accepted")

        // Alice wants to buy all Bob's assets for 1 Earth
        val order7 =
          matcherNode.placeOrder(aliceNode, bobEarthsPair, BUY, bobAssetQuantity, 1.earths)
        matcherNode.waitOrderStatus(bobEarthsPair, order7.message.id, "Filled")

        // Bob tries to do the same operation, but at now he have no assets
        matcherNode.expectIncorrectOrderPlacement(bobOrder, 400, "OrderRejected")
      }

      "trader can buy earths for assets with order without having earths" in {
        // Bob issues new asset
        val bobAssetQuantity = 10000
        val bobAssetName     = "BobCoin2"
        val bobAsset         = bobNode.issue(bobNode.address, bobAssetName, "Bob's asset", bobAssetQuantity, 0, false, 100000000L).id
        nodes.waitForHeightAriseAndTxPresent(bobAsset)

        val bobEarthsPair = AssetPair(
          amountAsset = ByteStr.decodeBase58(bobAsset).toOption,
          priceAsset = None
        )

        aliceNode.assertAssetBalance(aliceNode.address, bobAsset, 0)
        matcherNode.assertAssetBalance(matcherNode.address, bobAsset, 0)
        bobNode.assertAssetBalance(bobNode.address, bobAsset, bobAssetQuantity)

        // Bob wants to sell all own assets for 1 Earth
        def bobOrder = matcherNode.prepareOrder(bobNode, bobEarthsPair, SELL, bobAssetQuantity, 1.earths)

        val order8 = matcherNode.placeOrder(bobOrder)
        matcherNode.waitOrderStatus(bobEarthsPair, order8.message.id, "Accepted")

        // Bob moves all earths to Alice
        val h1              = matcherNode.height
        val bobBalance      = matcherNode.accountBalances(bobNode.address)._1
        val transferAmount  = bobBalance - TransactionFee
        val transferAliceId = bobNode.transfer(bobNode.address, aliceNode.address, transferAmount, TransactionFee, None, None).id
        nodes.waitForHeightAriseAndTxPresent(transferAliceId)

        matcherNode.accountBalances(bobNode.address)._1 shouldBe 0

        // Order should stay accepted
        matcherNode.waitForHeight(h1 + 5, 2.minutes)
        matcherNode.waitOrderStatus(bobEarthsPair, order8.message.id, "Accepted")

        // Cleanup
        nodes.waitForHeightArise()
        matcherNode.cancelOrder(bobNode, bobEarthsPair, order8.message.id).status should be("OrderCanceled")

        val transferBobId = aliceNode.transfer(aliceNode.address, bobNode.address, transferAmount, TransactionFee, None, None).id
        nodes.waitForHeightAriseAndTxPresent(transferBobId)
      }
    }
  }
}
