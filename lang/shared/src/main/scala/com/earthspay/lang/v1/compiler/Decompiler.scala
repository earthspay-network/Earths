package com.earthspay.lang.v1.compiler

import cats.implicits._
import com.earthspay.lang.contract.Contract
import com.earthspay.lang.contract.Contract.{CallableFunction, VerifierFunction}
import com.earthspay.lang.v1.FunctionHeader
import com.earthspay.lang.v1.compiler.Terms._
import monix.eval.Coeval

object Decompiler {
  private[lang] def pure[A](a: A) = Coeval.evalOnce(a)

  private def out(in: String, ident: Int): String =
    Array.fill(4 * ident)(" ").mkString("") + in

  private def pureOut(in: String, ident: Int): Coeval[String] = pure(out(in, ident))

  private val NEWLINE = scala.util.Properties.lineSeparator

  private def decl(e: Coeval[DECLARATION], ident: Int, ctx: DecompilerContext): Coeval[String] =
    e flatMap {
      case Terms.FUNC(name, args, body) =>
        expr(pure(body), 1, ctx).map(
          fb =>
            out("func " + name + " (" + args.mkString(",") + ") = {" + NEWLINE, ident) +
              out(fb + NEWLINE, ident) +
              out("}", ident))
      case Terms.LET(name, value) => expr(pure(value), 1 + ident, ctx).map(e => out("let " + name + " =" + NEWLINE + e, ident))
    }

  private[lang] def expr(e: Coeval[EXPR], ident: Int, ctx: DecompilerContext): Coeval[String] =
    e flatMap {
      case Terms.TRUE                 => pureOut("true", ident)
      case Terms.FALSE                => pureOut("false", ident)
      case Terms.CONST_BOOLEAN(b)     => pureOut(b.toString.toLowerCase(), ident)
      case Terms.CONST_LONG(t)        => pureOut(t.toLong.toString, ident)
      case Terms.CONST_STRING(s)      => pureOut('"' + s + '"', ident)
      case Terms.CONST_BYTESTR(bs)    => pureOut("base58'" + bs.base58 + "'", ident)
      case Terms.REF(ref)             => pureOut(ref, ident)
      case Terms.GETTER(getExpr, fld) => expr(pure(getExpr), ident, ctx).map(a => a + "." + fld)
      case Terms.IF(cond, it, iff) =>
        for {
          c   <- expr(pure(cond), 2 + ident, ctx)
          it  <- expr(pure(it), 2 + ident, ctx)
          iff <- expr(pure(iff), 2 + ident, ctx)
        } yield
          out("{" + NEWLINE, ident) +
            out("if (" + NEWLINE, 1 + ident) +
            out(c + NEWLINE, 0) +
            out(")" + NEWLINE, 1 + ident) +
            out("then" + NEWLINE, 1 + ident) +
            out(it + NEWLINE, 0) +
            out("else" + NEWLINE, 1 + ident) +
            out(iff + NEWLINE, 0) +
            out("}", ident)
      case Terms.BLOCK(declPar, body) =>
        for {
          d <- decl(pure(declPar), 1 + ident, ctx)
          b <- expr(pure(body), 1 + ident, ctx)
        } yield
          out("{" + NEWLINE, ident) +
            out(d + ";" + NEWLINE, 0) +
            out(b + NEWLINE, 0) +
            out("}", ident)

      case Terms.LET_BLOCK(let, exprPar) => expr(pure(Terms.BLOCK(let, exprPar)), ident, ctx)
      case Terms.FUNCTION_CALL(func, args) =>
        val argsCoeval = args
          .map(a => expr(pure(a), 0, ctx))
          .toVector
          .sequence

        func match {
          case FunctionHeader.Native(name) =>
            ctx.binaryOps.get(name) match {
              case Some(binOp) =>
                argsCoeval.map(as => out("(" + as.head + " " + binOp + " " + as.tail.head + ")", ident))
              case None =>
                argsCoeval.map(
                  as =>
                    out(ctx.opCodes.getOrElse(name, "Native<" + name + ">") + "(" + as.mkString(", ")
                          + ")",
                        ident))
            }

          case FunctionHeader.User(name) => argsCoeval.map(as => out(name + "(" + as.mkString(", ") + ")", ident))
        }
      case _: Terms.ARR     => ??? // never happens
      case _: Terms.CaseObj => ??? // never happens
    }

  def apply(e: Contract, ctx: DecompilerContext): String = {

    def intersperse(s: Seq[Coeval[String]]): Coeval[String] = s.toVector.sequence.map(v => v.mkString(NEWLINE + NEWLINE))

    import e._

    val decls: Seq[Coeval[String]] = dec.map(expr => decl(pure(expr), 0, ctx))
    val callables: Seq[Coeval[String]] = cfs
      .map {
        case CallableFunction(annotation, u) =>
          Decompiler.decl(pure(u), 0, ctx).map(out(NEWLINE + "@Callable(" + annotation.invocationArgName + ")" + NEWLINE, 0) + _)
      }

    val verifier: Seq[Coeval[String]] = vf.map {
      case VerifierFunction(annotation, u) =>
        Decompiler.decl(pure(u), 0, ctx).map(out(NEWLINE + "@Verifier(" + annotation.invocationArgName + ")" + NEWLINE, 0) + _)
    }.toSeq

    val result = for {
      d <- intersperse(decls)
      c <- intersperse(callables)
      v <- intersperse(verifier)
    } yield d + NEWLINE + c + NEWLINE + v

    result()
  }

  def apply(e0: EXPR, ctx: DecompilerContext): String =
    expr(pure(e0), 0, ctx).apply()

}
