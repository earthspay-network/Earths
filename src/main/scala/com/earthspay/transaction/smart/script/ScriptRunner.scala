package com.earthspay.transaction.smart.script

import cats.implicits._
import com.earthspay.account.AddressScheme
import com.earthspay.lang.v1.evaluator.EvaluatorV1
import com.earthspay.lang.{ExecutionError, ExprEvaluator}
import com.earthspay.state._
import com.earthspay.transaction.Transaction
import com.earthspay.transaction.smart.BlockchainContext
import monix.eval.Coeval

object ScriptRunner {

  def apply[A, T <: Transaction](height: Int, tx: T, blockchain: Blockchain, script: Script): (ExprEvaluator.Log, Either[ExecutionError, A]) =
    script match {
      case Script.Expr(expr) =>
        val ctx = BlockchainContext.build(
          AddressScheme.current.chainId,
          Coeval.evalOnce(tx),
          Coeval.evalOnce(height),
          blockchain
        )
        EvaluatorV1.applywithLogging[A](ctx, expr)

      case _ => (List.empty, "Unsupported script version".asLeft[A])
    }

}
