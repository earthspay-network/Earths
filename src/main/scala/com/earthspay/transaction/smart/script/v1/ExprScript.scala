package com.earthspay.transaction.smart.script.v1

import com.earthspay.common.state.ByteStr
import com.earthspay.crypto
import com.earthspay.lang.StdLibVersion._
import com.earthspay.lang.v1.compiler.Terms._
import com.earthspay.lang.v1.evaluator.FunctionIds._
import com.earthspay.lang.v1.{FunctionHeader, ScriptEstimator, Serde}
import com.earthspay.transaction.smart.script.Script
import com.earthspay.utils.{functionCosts, varNames}
import monix.eval.Coeval

import scala.annotation.tailrec
import scala.collection.mutable._

object ExprScript {
  val checksumLength         = 4
  private val maxComplexity  = 20 * functionCosts(V1)(FunctionHeader.Native(SIGVERIFY))()
  private val maxSizeInBytes = 8 * 1024

  def validateBytes(bs: Array[Byte]): Either[String, Unit] =
    Either.cond(bs.length <= maxSizeInBytes, (), s"Script is too large: ${bs.length} bytes > $maxSizeInBytes bytes")

  def apply(x: EXPR): Either[String, Script] = apply(V1, x)

  def apply(version: StdLibVersion, x: EXPR, checkSize: Boolean = true): Either[String, Script] =
    for {
      scriptComplexity <- ScriptEstimator(varNames(version), functionCosts(version), x)
      _                <- Either.cond(scriptComplexity <= maxComplexity, (), s"Script is too complex: $scriptComplexity > $maxComplexity")
      s = new ExprScriprImpl(version, x, scriptComplexity)
      _ <- if (checkSize) validateBytes(s.bytes().arr) else Right(())
    } yield s

  case class ExprScriprImpl(stdLibVersion: StdLibVersion, expr: EXPR, complexity: Long) extends Script {
    override type Expr = EXPR

    override val bytes: Coeval[ByteStr] =
      Coeval.evalOnce {
        val s = Array(stdLibVersion.toByte) ++ Serde.serialize(expr)
        ByteStr(s ++ crypto.secureHash(s).take(checksumLength))
      }
    override val containsBlockV2: Coeval[Boolean] = Coeval.evalOnce(isExprContainsBlockV2(expr))
  }

  def isExprContainsBlockV2(e: EXPR): Boolean = {
    @tailrec
    def horTraversal(queue: MutableList[EXPR]): Boolean = {
      queue.headOption match {
        case Some(expr) =>
          expr match {
            case BLOCK(_, _)                => true
            case GETTER(expr1, _)           => horTraversal(queue.tail += expr1)
            case LET_BLOCK(let, body)       => horTraversal(queue.tail ++ MutableList(let.value, body))
            case IF(expr1, expr2, expr3)    => horTraversal(queue.tail ++ MutableList(expr1, expr2, expr3))
            case FUNCTION_CALL(_, exprList) => horTraversal(queue.tail ++ exprList)
            case _                          => false
          }
        case None => false
      }
    }
    horTraversal(Queue(e))
  }
}
