package com.earthspay.matcher.market

import akka.actor.{Actor, Props}
import com.earthspay.matcher.MatcherSettings
import com.earthspay.matcher.api.BatchCancel
import com.earthspay.matcher.market.OrderHistoryActor._
import com.earthspay.matcher.model.Events.{OrderAdded, OrderCanceled, OrderExecuted}
import com.earthspay.matcher.model._
import com.earthspay.metrics.TimerExt
import com.earthspay.state.ByteStr
import kamon.Kamon
import org.iq80.leveldb.DB

class OrderHistoryActor(db: DB, settings: MatcherSettings) extends Actor {

  val orderHistory = new OrderHistory(db, settings)

  private val timer          = Kamon.timer("matcher.order-history")
  private val addedTimer     = timer.refine("event" -> "added")
  private val executedTimer  = timer.refine("event" -> "executed")
  private val cancelledTimer = timer.refine("event" -> "cancelled")

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[OrderAdded])
    context.system.eventStream.subscribe(self, classOf[OrderExecuted])
    context.system.eventStream.subscribe(self, classOf[OrderCanceled])
  }

  override def receive: Receive = {
    case ev: OrderAdded =>
      addedTimer.measure(orderHistory.process(ev))
    case ev: OrderExecuted =>
      executedTimer.measure(orderHistory.process(ev))
    case ev: OrderCanceled =>
      cancelledTimer.measure(orderHistory.process(ev))
    case ForceCancelOrderFromHistory(id) =>
      forceCancelOrder(id)
    case BatchCancel(address, _, ts) =>
      sender() ! orderHistory.updateTimestamp(address, ts)
  }

  def forceCancelOrder(id: ByteStr): Unit = {
    val maybeOrder = orderHistory.order(id)
    for (o <- maybeOrder) {
      val oi = orderHistory.orderInfo(id)
      orderHistory.process(OrderCanceled(LimitOrder.limitOrder(o.price, oi.remaining, oi.remainingFee, o), unmatchable = false))
    }
    sender ! maybeOrder
  }
}

object OrderHistoryActor {
  def name: String = "OrderHistory"

  def props(db: DB, settings: MatcherSettings): Props = Props(new OrderHistoryActor(db, settings))

  case class ForceCancelOrderFromHistory(orderId: ByteStr)
}
