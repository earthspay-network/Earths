package com.earthspay.http

import com.earthspay.api.http.ApiError
import com.earthspay.network._
import com.earthspay.transaction.{Transaction, ValidationError}
import com.earthspay.utx.UtxPool
import io.netty.channel.group.ChannelGroup

import scala.concurrent.Future

trait BroadcastRoute {
  def utx: UtxPool

  def allChannels: ChannelGroup

  import scala.concurrent.ExecutionContext.Implicits.global

  protected def doBroadcast(v: Either[ValidationError, Transaction]): Future[Either[ApiError, Transaction]] = Future {
    val r = for {
      tx <- v
      r  <- utx.putIfNew(tx)
    } yield {
      val (added, _) = r
      if (added) allChannels.broadcastTx(tx, None)
      tx
    }

    r.left.map(ApiError.fromValidationError)
  }
}
