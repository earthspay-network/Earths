package com.earthspay.transaction.smart.script

import cats.implicits._
import com.earthspay.account.AddressScheme
import com.earthspay.lang.v1.compiler.Terms.EVALUATED
import com.earthspay.lang.v1.evaluator.EvaluatorV1
import com.earthspay.lang._
import com.earthspay.lang.contract.Contract
import com.earthspay.lang.v1.evaluator._
import com.earthspay.lang.v1.compiler.Terms.{FALSE, TRUE}
import com.earthspay.state._
import com.earthspay.transaction.{Authorized, Proven}
import com.earthspay.transaction.smart.{BlockchainContext, RealTransactionWrapper, Verifier}
import com.earthspay.transaction.smart.script.v1.ExprScript.ExprScriprImpl
import monix.eval.Coeval

object ScriptRunner {
  type TxOrd = BlockchainContext.In

  def apply(height: Int, in: TxOrd, blockchain: Blockchain, script: Script, isTokenScript: Boolean): (Log, Either[ExecutionError, EVALUATED]) = {
    script match {
      case s: ExprScriprImpl =>
        val ctx = BlockchainContext.build(
          script.stdLibVersion,
          AddressScheme.current.chainId,
          Coeval.evalOnce(in),
          Coeval.evalOnce(height),
          blockchain,
          isTokenScript
        )
        EvaluatorV1.applywithLogging[EVALUATED](ctx, s.expr)
      case ContractScript.ContractScriptImpl(_, Contract(_, _, Some(vf)), _) =>
        val ctx = BlockchainContext.build(
          script.stdLibVersion,
          AddressScheme.current.chainId,
          Coeval.evalOnce(in),
          Coeval.evalOnce(height),
          blockchain,
          isTokenScript
        )
        val evalContract = in.eliminate(t => ContractEvaluator.verify(vf, RealTransactionWrapper.apply(t)),
                                        _.eliminate(t => ContractEvaluator.verify(vf, RealTransactionWrapper.ord(t)), _ => ???))
        EvaluatorV1.evalWithLogging(ctx, evalContract)

      case ContractScript.ContractScriptImpl(_, Contract(_, _, None), _) =>
        val t: Proven with Authorized =
          in.eliminate(_.asInstanceOf[Proven with Authorized], _.eliminate(_.asInstanceOf[Proven with Authorized], _ => ???))
        (List.empty, Verifier.verifyAsEllipticCurveSignature[Proven with Authorized](t) match {
          case Right(_) => Right(TRUE)
          case Left(_)  => Right(FALSE)
        })
      case _ => (List.empty, "Unsupported script version".asLeft[EVALUATED])
    }
  }
}
