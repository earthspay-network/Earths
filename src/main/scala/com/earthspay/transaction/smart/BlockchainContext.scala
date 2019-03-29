package com.earthspay.transaction.smart

import cats.kernel.Monoid
import com.earthspay.lang.Global
import com.earthspay.lang.StdLibVersion._
import com.earthspay.lang.v1.evaluator.ctx.EvaluationContext
import com.earthspay.lang.v1.evaluator.ctx.impl.earths.EarthsContext
import com.earthspay.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.earthspay.state._
import monix.eval.Coeval

object BlockchainContext {

  type In = EarthsEnvironment.In
  def build(version: StdLibVersion,
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
