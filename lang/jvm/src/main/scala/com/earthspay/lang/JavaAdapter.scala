package com.earthspay.lang

import cats.kernel.Monoid
import com.earthspay.lang.ScriptVersion.Versions.V1
import com.earthspay.lang.v1.compiler.CompilerV1
import com.earthspay.lang.v1.compiler.Terms.EXPR
import com.earthspay.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.earthspay.lang.v1.evaluator.ctx.impl.earths.EarthsContext

object JavaAdapter {
  private val version = V1

  lazy val compiler =
    new CompilerV1(
      Monoid.combineAll(Seq(
        CryptoContext.compilerContext(com.earthspay.lang.Global),
        EarthsContext.build(version, null, false).compilerContext,
        PureContext.build(version).compilerContext
      )))

  def compile(input: String): EXPR = {
    compiler
      .compile(input, List())
      .fold(
        error => throw new IllegalArgumentException(error),
        expr => expr
      )
  }
}
