package com.earthspay.state.diffs.smart.predef

import com.earthspay.TransactionGen
import com.earthspay.lang.StdLibVersion._
import com.earthspay.lang.Testing
import com.earthspay.lang.v1.compiler.ExpressionCompiler
import com.earthspay.lang.v1.compiler.Terms.EVALUATED
import com.earthspay.lang.v1.parser.Parser
import com.earthspay.state.Blockchain
import com.earthspay.state.diffs._
import com.earthspay.transaction.Transaction
import com.earthspay.transaction.smart.script.ScriptRunner
import com.earthspay.transaction.smart.script.v1.ExprScript
import com.earthspay.utils.{EmptyBlockchain, compilerContext}
import fastparse.core.Parsed.Success
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FreeSpec, Matchers}
import shapeless.Coproduct

class ScriptVersionsTest extends FreeSpec with PropertyChecks with Matchers with TransactionGen {
  def eval[T <: EVALUATED](script: String,
                           version: StdLibVersion,
                           tx: Transaction = null,
                           blockchain: Blockchain = EmptyBlockchain): Either[String, EVALUATED] = {
    val Success(expr, _) = Parser.parseExpr(script)
    for {
      compileResult <- ExpressionCompiler(compilerContext(version, isAssetScript = false), expr)
      (typedExpr, _) = compileResult
      s <- ExprScript(version, typedExpr, checkSize = false)
      r <- ScriptRunner(blockchain.height, Coproduct(tx), blockchain, s, isTokenScript = false)._2
    } yield r

  }

  val duplicateNames =
    """
      |match tx {
      |  case tx: TransferTransaction => true
      |  case _ => false
      |}
    """.stripMargin

  val orderTypeBindings = "let t = Buy; t == Buy"

  "ScriptV1 allows duplicate names" in {
    forAll(transferV2Gen.flatMap(tx => Gen.oneOf(V1, V2).map(v => (tx, v)))) {
      case (tx, v) =>
        eval[EVALUATED](duplicateNames, v, tx) shouldBe Testing.evaluated(true)
    }
  }

  "ScriptV1 - does not have bindings defined in V2" in {
    eval[EVALUATED](orderTypeBindings, V1) should produce("definition of 'Buy' is not found")
  }

  "ScriptV2" - {
    "allows duplicate names" in {
      forAll(transferV2Gen) { tx =>
        eval[EVALUATED](duplicateNames, V2, tx) shouldBe Testing.evaluated(true)
      }
    }

    "has bindings defined in V2" in {
      eval[EVALUATED](orderTypeBindings, V2) shouldBe Testing.evaluated(true)
    }

  }
}
