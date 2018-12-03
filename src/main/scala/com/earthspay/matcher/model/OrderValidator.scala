package com.earthspay.matcher.model

import cats.implicits._
import com.earthspay.account.{Address, PublicKeyAccount}
import com.earthspay.matcher.MatcherSettings
import com.earthspay.matcher.api.DBUtils
import com.earthspay.matcher.api.DBUtils.indexes.active.MaxElements
import com.earthspay.matcher.model.OrderHistory.OrderInfoChange
import com.earthspay.metrics.TimerExt
import com.earthspay.state._
import com.earthspay.transaction.assets.exchange.{AssetPair, Order}
import com.earthspay.transaction.{AssetAcc, AssetId}
import com.earthspay.utils.Time
import kamon.Kamon
import org.iq80.leveldb.DB

/**
  * @param db matcher LevelDB instance
  * @param portfolio "pessimistic" portfolio, which includes pending spendings from UTX pool!
  */
class OrderValidator(db: DB,
                     blockchain: Blockchain,
                     portfolio: Address => Portfolio,
                     validatePair: AssetPair => Either[String, AssetPair],
                     settings: MatcherSettings,
                     val matcherPublicKey: PublicKeyAccount,
                     time: Time) {
  import OrderValidator._

  private val timer = Kamon.timer("matcher.validation")

  private def spendableBalance(a: AssetAcc): Long = {
    val p = portfolio(a.account)
    a.assetId match {
      case Some(x) => p.assets.getOrElse(x, 0)
      case None    => p.spendableBalance
    }
  }

  private def validateBalance(o: Order): Either[String, Order] = {
    val senderAddress = o.sender.toAddress
    val lo            = LimitOrder(o)
    val actualBalance = Set(lo.feeAsset, lo.spentAsset).map(assetId => assetId -> spendableBalance(AssetAcc(senderAddress, assetId))).toMap
    val openVolume    = actualBalance.map { case (assetId, _) => assetId -> DBUtils.openVolume(db, senderAddress, assetId) }
    val change        = OrderInfoChange(o, None, OrderInfo(o.amount, 0L, None, None, o.matcherFee, Some(0L)))
    val newOrder      = OrderHistory.diff(List(change)).getOrElse(senderAddress, OpenPortfolio.empty)
    val needs         = OpenPortfolio(openVolume).combine(newOrder)

    Either.cond(
      actualBalance.combine(needs.orders.mapValues(-_)).forall(_._2 >= 0),
      o,
      s"Not enough tradable balance. Order requires ${formatPortfolio(newOrder.orders)}, " +
        s"available balance is ${formatPortfolio(actualBalance.combine(openVolume.mapValues(-_)))}"
    )
  }

  @inline private def decimals(assetId: Option[AssetId]): Either[String, Int] = assetId.fold[Either[String, Int]](Right(8)) { aid =>
    blockchain.assetDescription(aid).map(_.decimals).toRight(s"Invalid asset id $aid")
  }

  private def validateDecimals(o: Order): Either[String, Order] =
    for {
      pd <- decimals(o.assetPair.priceAsset)
      ad <- decimals(o.assetPair.amountAsset)
      insignificantDecimals = (pd - ad).max(0)
      _ <- Either.cond(o.price % BigDecimal(10).pow(insignificantDecimals).toLongExact == 0,
                       o,
                       s"Invalid price, last $insignificantDecimals digits must be 0")
    } yield o

  def tradableBalance(acc: AssetAcc): Long =
    timer
      .refine("action" -> "tradableBalance")
      .measure {
        math.max(0l, spendableBalance(acc) - DBUtils.openVolume(db, acc.account, acc.assetId))
      }

  def validateNewOrder(order: Order): Either[String, Order] =
    timer
      .refine("action" -> "place", "pair" -> order.assetPair.toString)
      .measure {
        lazy val senderAddress = order.sender.toAddress
        lazy val lowestOrderTs = DBUtils
          .lastOrderTimestamp(db, order.senderPublicKey)
          .getOrElse(settings.defaultOrderTimestamp) - settings.orderTimestampDrift

        for {
          _ <- (Right(order): Either[String, Order])
            .ensure("Incorrect matcher public key")(_.matcherPublicKey == matcherPublicKey)
            .ensure("Invalid address")(_ => !settings.blacklistedAddresses.contains(senderAddress))
            .ensure("Order expiration should be > 1 min")(_.expiration > time.correctedTime() + MinExpiration)
            .ensure(s"Order should have a timestamp after $lowestOrderTs, but it is ${order.timestamp}")(_.timestamp > lowestOrderTs)
            .ensure(s"Order matcherFee should be >= ${settings.minOrderFee}")(_.matcherFee >= settings.minOrderFee)
            .ensure("Invalid signature")(_.signatureValid())
          _ <- order.isValid(time.correctedTime()).toEither
          _ <- (Right(order): Either[String, Order])
            .ensure("Order has already been placed")(o => DBUtils.orderInfo(db, o.id()).status == LimitOrder.NotFound)
            .ensure(s"Limit of $MaxElements active orders has been reached")(o => DBUtils.activeOrderCount(db, o.senderPublicKey) < MaxElements)
            .ensure("Trading on scripted account isn't allowed yet")(_ => !blockchain.hasScript(senderAddress))
          _ <- validateBalance(order)
          _ <- validatePair(order.assetPair)
          _ <- validateDecimals(order)
        } yield order
      }
}

object OrderValidator {
  val MinExpiration: Long = 60 * 1000L

  private def formatPortfolio(m: Map[Option[AssetId], Long]): String =
    m.map {
      case (assetId, v) => s"${AssetPair.assetIdStr(assetId)} -> $v"
    } mkString ("[", ", ", "]")
}
