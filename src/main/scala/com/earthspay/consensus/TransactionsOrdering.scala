package com.earthspay.consensus

import com.earthspay.transaction.Transaction

object TransactionsOrdering {
  trait EarthsOrdering extends Ordering[Transaction] {
    def txTimestampOrder(ts: Long): Long
    private def orderBy(t: Transaction): (Double, Long, Long) = {
      val size        = t.bytes().size
      val byFee       = if (t.assetFee._1.nonEmpty) 0 else -t.assetFee._2
      val byTimestamp = txTimestampOrder(t.timestamp)

      (byFee.toDouble / size.toDouble, byFee, byTimestamp)
    }
    override def compare(first: Transaction, second: Transaction): Int = {
      implicitly[Ordering[(Double, Long, Long)]].compare(orderBy(first), orderBy(second))
    }
  }

  object InBlock extends EarthsOrdering {
    // sorting from network start
    override def txTimestampOrder(ts: Long): Long = -ts
  }

  object InUTXPool extends EarthsOrdering {
    override def txTimestampOrder(ts: Long): Long = ts
  }
}