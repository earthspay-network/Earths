package com.earthspay.matcher.model

import com.earthspay.account.PrivateKeyAccount
import com.earthspay.matcher.MatcherSettings
import com.earthspay.matcher.model.Events.OrderExecuted
import com.earthspay.transaction.ValidationError
import com.earthspay.transaction.assets.exchange.{ExchangeTransaction, Order}
import com.earthspay.utils.Time

class ExchangeTransactionCreator(matcherPrivateKey: PrivateKeyAccount, settings: MatcherSettings, time: Time) {
  private def calculateMatcherFee(buy: Order, sell: Order, amount: Long): (Long, Long) = {
    def calcFee(o: Order, amount: Long): Long = {
      val p = BigInt(amount) * o.matcherFee / o.amount
      p.toLong
    }

    (calcFee(buy, amount), calcFee(sell, amount))
  }

  def createTransaction(event: OrderExecuted): Either[ValidationError, ExchangeTransaction] = {
    import event.{counter, submitted}
    val price             = counter.price
    val (buy, sell)       = Order.splitByType(submitted.order, counter.order)
    val (buyFee, sellFee) = calculateMatcherFee(buy, sell, event.executedAmount)
    ExchangeTransaction.create(matcherPrivateKey,
                               buy,
                               sell,
                               event.executedAmount,
                               price,
                               buyFee,
                               sellFee,
                               settings.orderMatchTxFee,
                               time.getTimestamp())
  }
}
