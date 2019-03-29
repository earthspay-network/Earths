package com.earthspay.matcher.smart

import cats.implicits._
import com.earthspay.account.AddressScheme
import com.earthspay.lang.contract.Contract
import com.earthspay.lang.v1.compiler.Terms.{EVALUATED, FALSE, TRUE}
import com.earthspay.lang.v1.evaluator.{ContractEvaluator, EvaluatorV1, Log}
import com.earthspay.transaction.{Authorized, Proven}
import com.earthspay.transaction.assets.exchange.Order
import com.earthspay.transaction.smart.{RealTransactionWrapper, Verifier}
import com.earthspay.transaction.smart.script.{ContractScript, Script}
import com.earthspay.transaction.smart.script.v1.ExprScript.ExprScriprImpl
import monix.eval.Coeval

object MatcherScriptRunner {

  def apply(script: Script, order: Order, isTokenScript: Boolean): (Log, Either[String, EVALUATED]) = script match {
    case s: ExprScriprImpl =>
      val ctx = MatcherContext.build(script.stdLibVersion, AddressScheme.current.chainId, Coeval.evalOnce(order), !isTokenScript)
      EvaluatorV1.applywithLogging(ctx, s.expr)

    case ContractScript.ContractScriptImpl(_, Contract(_, _, Some(vf)), _) =>
      val ctx = MatcherContext.build(
        script.stdLibVersion,
        AddressScheme.current.chainId,
        Coeval.evalOnce(???) /*order not used in global context where @Verifier annotation is used */,
        proofsEnabled = true
      )
      val evalContract = ContractEvaluator.verify(vf, RealTransactionWrapper.ord(order))
      EvaluatorV1.evalWithLogging(ctx, evalContract)

    case ContractScript.ContractScriptImpl(_, Contract(_, _, None), _) =>
      (List.empty, Verifier.verifyAsEllipticCurveSignature[Proven with Authorized](order) match {
        case Right(_) => Right(TRUE)
        case Left(_)  => Right(FALSE)
      })
    case _ => (List.empty, "Unsupported script version".asLeft[EVALUATED])
  }
}
