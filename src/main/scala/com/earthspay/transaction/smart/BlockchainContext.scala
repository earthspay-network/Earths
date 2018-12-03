package com.earthspay.transaction.smart

import cats.kernel.Monoid
import com.earthspay.lang.Global
import com.earthspay.lang.v1.evaluator.ctx.EvaluationContext
import com.earthspay.lang.v1.evaluator.ctx.impl.earths.EarthsContext
import com.earthspay.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.earthspay.state._
import monix.eval.Coeval
import com.earthspay.transaction._

object BlockchainContext {

  private val baseContext = Monoid.combine(PureContext.ctx, CryptoContext.build(Global)).evaluationContext

  def build(nByte: Byte, tx: Coeval[Transaction], h: Coeval[Int], blockchain: Blockchain): EvaluationContext =
    Monoid.combine(baseContext, EarthsContext.build(new EarthsEnvironment(nByte, tx, h, blockchain)).evaluationContext)
}
