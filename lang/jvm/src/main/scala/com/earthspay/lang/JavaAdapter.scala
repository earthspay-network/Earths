package com.earthspay.lang

import cats.kernel.Monoid
import com.earthspay.lang.StdLibVersion.V2
import com.earthspay.lang.v1.compiler.ExpressionCompiler
import com.earthspay.lang.v1.compiler.Terms.EXPR
import com.earthspay.lang.v1.evaluator.ctx.impl.earths.EarthsContext
import com.earthspay.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}

object JavaAdapter {
  private val version = V2

  lazy val ctx =
    Monoid.combineAll(
      Seq(
        CryptoContext.compilerContext(com.earthspay.lang.Global),
        EarthsContext.build(version, null, false).compilerContext,
        PureContext.build(version).compilerContext
      ))

  def compile(input: String): EXPR = {
    ExpressionCompiler
      .compile(input, ctx)
      .fold(
        error => throw new IllegalArgumentException(error),
        expr => expr
      )
  }
}
