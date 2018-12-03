package com.earthspay.matcher.market

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference

import akka.actor.{ActorRef, Props}
import akka.testkit.ImplicitSender
import com.earthspay.account.{PrivateKeyAccount, PublicKeyAccount}
import com.earthspay.matcher.api.OrderAccepted
import com.earthspay.matcher.fixtures.RestartableActor
import com.earthspay.matcher.market.MatcherActor.{GetMarkets, MarketData}
import com.earthspay.matcher.market.OrderBookActor.MarketStatus
import com.earthspay.matcher.model.{ExchangeTransactionCreator, OrderBook}
import com.earthspay.matcher.{AssetPairBuilder, MatcherTestData}
import com.earthspay.state.{AssetDescription, Blockchain, ByteStr, LeaseBalance, Portfolio}
import com.earthspay.transaction.AssetId
import com.earthspay.transaction.assets.IssueTransactionV1
import com.earthspay.transaction.assets.exchange.AssetPair
import com.earthspay.utx.UtxPool
import io.netty.channel.group.ChannelGroup
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.BeforeAndAfterEach

class MatcherActorSpecification
    extends MatcherSpec("MatcherActor")
    with MatcherTestData
    with BeforeAndAfterEach
    with PathMockFactory
    with ImplicitSender {

  private val blockchain: Blockchain = stub[Blockchain]
  (blockchain.assetDescription _)
    .when(ByteStr.decodeBase58("BASE1").get)
    .returns(Some(AssetDescription(PrivateKeyAccount(Array.empty), "Unknown".getBytes, Array.emptyByteArray, 8, false, 1, None, 0)))
  (blockchain.assetDescription _)
    .when(ByteStr.decodeBase58("BASE2").get)
    .returns(Some(AssetDescription(PrivateKeyAccount(Array.empty), "Unknown".getBytes, Array.emptyByteArray, 8, false, 1, None, 0)))

  private val pairBuilder = new AssetPairBuilder(matcherSettings, blockchain)
  private val txFactory   = new ExchangeTransactionCreator(MatcherAccount, matcherSettings, ntpTime).createTransaction _
  private val obc         = new ConcurrentHashMap[AssetPair, OrderBook]
  private val ob          = new AtomicReference(Map.empty[AssetPair, ActorRef])
  private val md          = new ConcurrentHashMap[AssetPair, MarketStatus]

  def update(ap: AssetPair)(snapshot: OrderBook): Unit = obc.put(ap, snapshot)

  private def defaultActor: ActorRef =
    system.actorOf(
      Props(
        new MatcherActor(pairBuilder.validateAssetPair,
                         ob,
                         update,
                         p => m => md.put(p, m),
                         mock[UtxPool],
                         mock[ChannelGroup],
                         matcherSettings,
                         blockchain.assetDescription,
                         txFactory) with RestartableActor))

  private var actor: ActorRef = defaultActor

  val i1 = IssueTransactionV1
    .selfSigned(PrivateKeyAccount(Array.empty), "Unknown".getBytes(), Array.empty, 10000000000L, 8.toByte, true, 100000L, 10000L)
    .right
    .get
  val i2 = IssueTransactionV1
    .selfSigned(PrivateKeyAccount(Array.empty), "ForbiddenName".getBytes(), Array.empty, 10000000000L, 8.toByte, true, 100000L, 10000L)
    .right
    .get
  (blockchain.assetDescription _)
    .when(i2.id())
    .returns(Some(AssetDescription(i2.sender, "ForbiddenName".getBytes, "".getBytes, 8, false, i2.quantity, None, 0)))
  (blockchain.assetDescription _)
    .when(*)
    .returns(Some(AssetDescription(PublicKeyAccount(Array(0: Byte)), "Unknown".getBytes, "".getBytes, 8, false, i1.quantity, None, 0)))
  (blockchain.portfolio _).when(*).returns(Portfolio(Long.MaxValue, LeaseBalance.empty, Map(ByteStr("123".getBytes) -> Long.MaxValue)))

  override protected def beforeEach(): Unit = {
    obc.clear()
    md.clear()
    super.beforeEach()
    actor = defaultActor
  }

  "MatcherActor" should {
    "return all open markets" in {
      val a1 = strToSomeAssetId("123")
      val a2 = strToSomeAssetId("234")

      val pair  = AssetPair(a2, a1)
      val order = buy(pair, 2000, 1)

      actor ! order
      expectMsg(OrderAccepted(order))

      actor ! GetMarkets

      expectMsgPF() {
        case s @ Seq(MarketData(_, "Unknown", "Unknown", _, _, _)) =>
          s.size shouldBe 1
      }
    }
    "delete order books" is pending
    "forward new orders to order books" is pending
  }

  override protected def afterAll(): Unit          = shutdown()
  def strToSomeAssetId(s: String): Option[AssetId] = Some(ByteStr(s.getBytes()))
}
