package com.earthspay.matcher.market

import com.earthspay.WithDB
import com.earthspay.matcher.model._
import com.earthspay.matcher.{MatcherSettings, MatcherTestData}
import com.earthspay.settings.{Constants, WalletSettings}
import com.earthspay.state.{Blockchain, ByteStr, EitherExt2, LeaseBalance, Portfolio}
import com.earthspay.utx.UtxPool
import org.scalamock.scalatest.PathMockFactory
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import com.earthspay.account.{PrivateKeyAccount, PublicKeyAccount}
import com.earthspay.transaction.ValidationError
import com.earthspay.transaction.assets.IssueTransactionV1
import com.earthspay.transaction.assets.exchange.{AssetPair, Order}
import com.earthspay.wallet.Wallet

class OrderValidatorSpecification
    extends WordSpec
    with WithDB
    with PropertyChecks
    with Matchers
    with MatcherTestData
    with BeforeAndAfterAll
    with BeforeAndAfterEach
    with PathMockFactory {

  val utxPool: UtxPool = stub[UtxPool]

  val bc: Blockchain = stub[Blockchain]
  val i1: IssueTransactionV1 =
    IssueTransactionV1
      .selfSigned(PrivateKeyAccount(Array.empty), "WBTC".getBytes(), Array.empty, 10000000000L, 8.toByte, true, 100000L, 10000L)
      .right
      .get
  (bc.transactionInfo _).when(*).returns(Some((1, i1)))

  val s: MatcherSettings             = matcherSettings.copy(account = MatcherAccount.address)
  val w                              = Wallet(WalletSettings(None, "matcher", Some(WalletSeed)))
  val acc: Option[PrivateKeyAccount] = w.generateNewAccount()

  val matcherPubKey: PublicKeyAccount = w.findPrivateKey(s.account).explicitGet()

  private var ov = new OrderValidator {
    override val orderHistory: OrderHistory = new OrderHistory(db, matcherSettings)
    override val utxPool: UtxPool           = stub[UtxPool]
    override val settings: MatcherSettings  = s
    override val wallet: Wallet             = w
  }

  override def beforeEach(): Unit = {
    super.beforeEach()
    ov = new OrderValidator {
      override val orderHistory: OrderHistory = new OrderHistory(db, matcherSettings)
      override val utxPool: UtxPool           = stub[UtxPool]
      override val settings: MatcherSettings  = s
      override val wallet: Wallet             = w
    }
  }

  val wbtc         = ByteStr("WBTC".getBytes)
  val pairEarthsBtc = AssetPair(None, Some(wbtc))

  "OrderValidator" should {
    "allows buy EARTHS for BTC without balance for order fee" in {
      validateNewOrderTest(
        Portfolio(0,
                  LeaseBalance.empty,
                  Map(
                    wbtc -> 10 * Constants.UnitsInEarth
                  ))) shouldBe an[Right[_, _]]
    }

    "does not allow buy EARTHS for BTC when assets number is negative" in {
      validateNewOrderTest(
        Portfolio(0,
                  LeaseBalance.empty,
                  Map(
                    wbtc -> -10 * Constants.UnitsInEarth
                  ))) shouldBe a[Left[_, _]]
    }
  }

  private def validateNewOrderTest(expectedPortfolio: Portfolio): Either[ValidationError.GenericError, Order] = {
    (ov.utxPool.portfolio _).when(*).returns(expectedPortfolio)
    val o = buy(
      pair = pairEarthsBtc,
      price = 0.0022,
      amount = 100 * Constants.UnitsInEarth,
      matcherFee = Some((0.003 * Constants.UnitsInEarth).toLong)
    )
    ov.validateNewOrder(o)
  }
}
