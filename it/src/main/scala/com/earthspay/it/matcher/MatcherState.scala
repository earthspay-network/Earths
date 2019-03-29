package com.earthspay.it.matcher

import com.earthspay.account.PrivateKeyAccount
import com.earthspay.it.api.{MatcherStatusResponse, OrderBookResponse, OrderbookHistory}
import com.earthspay.matcher.queue.QueueEventWithMeta
import com.earthspay.transaction.assets.exchange.AssetPair

case class MatcherState(offset: QueueEventWithMeta.Offset,
                        snapshots: Map[String, QueueEventWithMeta.Offset],
                        orderBooks: Map[AssetPair, OrderBookResponse],
                        orderStatuses: Map[String, MatcherStatusResponse],
                        reservedBalances: Map[PrivateKeyAccount, Map[String, Long]],
                        orderHistory: Map[PrivateKeyAccount, Map[AssetPair, Seq[OrderbookHistory]]])
