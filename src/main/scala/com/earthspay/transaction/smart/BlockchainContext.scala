package com.earthspay.transaction.smart

import cats.kernel.Monoid
import com.earthspay.lang.{Global, ScriptVersion}
import com.earthspay.lang.v1.evaluator.ctx.EvaluationContext
import com.earthspay.lang.v1.evaluator.ctx.impl.earths.EarthsContext
import com.earthspay.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.earthspay.state._
import com.earthspay.transaction._
import com.earthspay.transaction.assets.exchange.Order
import monix.eval.Coeval
import shapeless._

object BlockchainContext {

  type In = Transaction :+: Order :+: CNil
  def build(version: ScriptVersion,
            nByte: Byte,
            in: Coeval[In],
            h: Coeval[Int],
            blockchain: Blockchain,
            isTokenContext: Boolean): EvaluationContext = {
    Monoid
      .combineAll(
        Seq(
          PureContext.build(version),
          CryptoContext.build(Global),
          EarthsContext.build(version, new EarthsEnvironment(nByte, in, h, blockchain), isTokenContext)
        ))
      .evaluationContext
  }
}
