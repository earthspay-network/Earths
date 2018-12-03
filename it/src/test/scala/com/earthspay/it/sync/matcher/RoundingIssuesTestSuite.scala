package com.earthspay.it.sync.matcher

import com.typesafe.config.Config
import com.earthspay.it.ReportingTestName
import com.earthspay.it.api.LevelResponse
import com.earthspay.it.api.SyncHttpApi._
import com.earthspay.it.api.SyncMatcherHttpApi._
import com.earthspay.it.sync.matcher.config.MatcherPriceAssetConfig._
import com.earthspay.it.transactions.NodesFromDocker
import com.earthspay.transaction.assets.exchange.OrderType
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.duration._

class RoundingIssuesTestSuite
    extends FreeSpec
    with Matchers
    with BeforeAndAfterAll
    with CancelAfterFailure
    with NodesFromDocker
    with ReportingTestName {

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head

  private def aliceNode = nodes(1)

  private def bobNode = nodes(2)

  Seq(IssueUsdTx, IssueEthTx, IssueBtcTx).map(createSignedIssueRequest).foreach(matcherNode.signedIssue)
  nodes.waitForHeightArise()

  "should correctly fill an order with small amount" in {
    val aliceBalanceBefore = matcherNode.accountBalances(aliceNode.address)._1
    val bobBalanceBefore   = matcherNode.accountBalances(bobNode.address)._1

    val counter   = matcherNode.prepareOrder(aliceNode, earthsUsdPair, OrderType.BUY, 3100000000L, 238)
    val counterId = matcherNode.placeOrder(counter).message.id

    val submitted   = matcherNode.prepareOrder(bobNode, earthsUsdPair, OrderType.SELL, 425532L, 235)
    val submittedId = matcherNode.placeOrder(submitted).message.id

    val filledAmount = 420169L
    matcherNode.waitOrderStatusAndAmount(earthsUsdPair, submittedId, "Filled", Some(filledAmount), 1.minute)
    matcherNode.waitOrderStatusAndAmount(earthsUsdPair, counterId, "PartiallyFilled", Some(filledAmount), 1.minute)

    matcherNode.cancelOrder(aliceNode, earthsUsdPair, counterId)
    val tx = matcherNode.transactionsByOrder(counterId).head

    matcherNode.waitForTransaction(tx.id)
    val rawExchangeTx = matcherNode.rawTransactionInfo(tx.id)

    (rawExchangeTx \ "price").as[Long] shouldBe counter.price
    (rawExchangeTx \ "amount").as[Long] shouldBe filledAmount
    (rawExchangeTx \ "buyMatcherFee").as[Long] shouldBe 40L
    (rawExchangeTx \ "sellMatcherFee").as[Long] shouldBe 296219L

    val aliceBalanceAfter = matcherNode.accountBalances(aliceNode.address)._1
    val bobBalanceAfter   = matcherNode.accountBalances(bobNode.address)._1

    (aliceBalanceAfter - aliceBalanceBefore) shouldBe (-40L + 420169L)
    (bobBalanceAfter - bobBalanceBefore) shouldBe (-296219L - 420169L)
  }

  "reserved balance should not be negative" in {
    val counter   = matcherNode.prepareOrder(bobNode, ethBtcPair, OrderType.BUY, 923431000L, 31887L)
    val counterId = matcherNode.placeOrder(counter).message.id

    val submitted   = matcherNode.prepareOrder(aliceNode, ethBtcPair, OrderType.SELL, 223345000L, 31887L)
    val submittedId = matcherNode.placeOrder(submitted).message.id

    val filledAmount = 223344937L
    matcherNode.waitOrderStatusAndAmount(ethBtcPair, submittedId, "Filled", Some(filledAmount), 1.minute)
    matcherNode.waitOrderStatusAndAmount(ethBtcPair, counterId, "PartiallyFilled", Some(filledAmount), 1.minute)

    withClue("Alice's reserved balance before cancel")(matcherNode.reservedBalance(aliceNode) shouldBe empty)

    matcherNode.cancelOrder(bobNode, ethBtcPair, counterId)
    val tx = matcherNode.transactionsByOrder(counterId).head

    matcherNode.waitForTransaction(tx.id)

    withClue("Bob's reserved balance after cancel")(matcherNode.reservedBalance(bobNode) shouldBe empty)
  }

  "should correctly fill 2 counter orders" in {
    val counter1 = matcherNode.prepareOrder(bobNode, earthsUsdPair, OrderType.SELL, 98333333L, 60L)
    matcherNode.placeOrder(counter1).message.id

    val counter2   = matcherNode.prepareOrder(bobNode, earthsUsdPair, OrderType.SELL, 100000000L, 70L)
    val counter2Id = matcherNode.placeOrder(counter2).message.id

    val submitted   = matcherNode.prepareOrder(aliceNode, earthsUsdPair, OrderType.BUY, 100000000L, 1000L)
    val submittedId = matcherNode.placeOrder(submitted).message.id

    matcherNode.waitOrderStatusAndAmount(earthsUsdPair, counter2Id, "PartiallyFilled", Some(2857143L), 1.minute)
    matcherNode.waitOrderStatusAndAmount(earthsUsdPair, submittedId, "Filled", Some(99523810L), 1.minute)

    withClue("orderBook check") {
      val ob = matcherNode.orderBook(earthsUsdPair)
      ob.bids shouldBe empty
      ob.asks shouldBe List(LevelResponse(97142857L, 70L)) // = 100000000 - 2857143
    }
  }

}
