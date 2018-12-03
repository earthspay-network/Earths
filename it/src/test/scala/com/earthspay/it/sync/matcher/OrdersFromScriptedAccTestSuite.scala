package com.earthspay.it.sync.matcher

import com.typesafe.config.{Config, ConfigFactory}
import com.earthspay.it._
import com.earthspay.it.api.SyncHttpApi._
import com.earthspay.it.api.SyncMatcherHttpApi._
import com.earthspay.it.transactions.NodesFromDocker
import com.earthspay.it.util._
import com.earthspay.state.ByteStr
import com.earthspay.transaction.assets.exchange.{AssetPair, Order, OrderType}
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}
import com.earthspay.it.sync._
import com.earthspay.lang.v1.compiler.CompilerV1
import com.earthspay.lang.v1.parser.Parser
import com.earthspay.transaction.smart.SetScriptTransaction
import com.earthspay.transaction.smart.script.v1.ScriptV1
import com.earthspay.utils.dummyCompilerContext
import play.api.libs.json.JsNumber

import scala.concurrent.duration._
import scala.util.Random
import com.earthspay.it.sync.matcher.config.MatcherDefaultConfig._

class OrdersFromScriptedAccTestSuite
    extends FreeSpec
    with Matchers
    with BeforeAndAfterAll
    with CancelAfterFailure
    with ReportingTestName
    with NodesFromDocker {

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head

  private def aliceNode = nodes(1)
  private def bobNode   = nodes(2)

  "issue asset and run test" - {
    // Alice issues new asset
    val aliceAsset =
      aliceNode.issue(aliceNode.address, "AliceCoin", "AliceCoin for matcher's tests", someAssetAmount, 0, reissuable = false, 100000000L).id
    nodes.waitForHeightAriseAndTxPresent(aliceAsset)
    val aliceEarthsPair = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)

    // check assets's balances
    aliceNode.assertAssetBalance(aliceNode.address, aliceAsset, someAssetAmount)
    aliceNode.assertAssetBalance(matcherNode.address, aliceAsset, 0)

    "make Bob's address as scripted" in {
      val scriptText = {
        val sc = Parser(s"""true""".stripMargin).get.value
        CompilerV1(dummyCompilerContext, sc).explicitGet()._1
      }

      val script = ScriptV1(scriptText).explicitGet()
      val setScriptTransaction = SetScriptTransaction
        .selfSigned(SetScriptTransaction.supportedVersions.head, bobNode.privateKey, Some(script), minFee, System.currentTimeMillis())
        .right
        .get

      val setScriptId = bobNode
        .signedBroadcast(setScriptTransaction.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt)))
        .id

      nodes.waitForHeightAriseAndTxPresent(setScriptId)

    }

    "Alice place sell order, but Bob cannot place order, because his acc is scripted" in {
      // Alice places sell order
      val aliceOrder = matcherNode
        .placeOrder(aliceNode, aliceEarthsPair, OrderType.SELL, 2.earths * Order.PriceConstant, 500, 10.minutes)

      aliceOrder.status shouldBe "OrderAccepted"

      val orderId = aliceOrder.message.id

      // Alice checks that the order in order book
      matcherNode.waitOrderStatus(aliceEarthsPair, orderId, "Accepted")
      matcherNode.fullOrderHistory(aliceNode).head.status shouldBe "Accepted"

      // Alice check that order is correct
      val orders = matcherNode.orderBook(aliceEarthsPair)
      orders.asks.head.amount shouldBe 500
      orders.asks.head.price shouldBe 2.earths * Order.PriceConstant

      // sell order should be in the aliceNode orderbook
      matcherNode.fullOrderHistory(aliceNode).head.status shouldBe "Accepted"

      // Bob gets error message
      assertBadRequestAndResponse(
        matcherNode
          .placeOrder(bobNode, aliceEarthsPair, OrderType.BUY, 2.earths * Order.PriceConstant, 500, 10.minutes),
        "Trading on scripted account isn't allowed yet."
      )

    }
  }

}
