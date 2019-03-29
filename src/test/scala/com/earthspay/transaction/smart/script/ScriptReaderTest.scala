package com.earthspay.transaction.smart.script

import com.earthspay.common.utils.EitherExt2
import com.earthspay.crypto
import com.earthspay.lang.StdLibVersion.{V3, V1}
import com.earthspay.lang.contract.Contract
import com.earthspay.lang.contract.Contract.{CallableAnnotation, CallableFunction}
import com.earthspay.lang.v1.Serde
import com.earthspay.lang.v1.compiler.Terms
import com.earthspay.lang.v1.compiler.Terms.TRUE
import com.earthspay.state.diffs.produce
import org.scalatest.{FreeSpec, Matchers}

class ScriptReaderTest extends FreeSpec with Matchers {
  val checksumLength = 4

  "should parse all bytes for V1" in {
    val body     = Array(V1.toByte) ++ Serde.serialize(TRUE) ++ "foo".getBytes
    val allBytes = body ++ crypto.secureHash(body).take(checksumLength)
    ScriptReader.fromBytes(allBytes) should produce("bytes left")
  }

  "should parse all bytes for V3" in {
    val sc = ContractScript(
      V3,
      Contract(List.empty, List(CallableFunction(CallableAnnotation("sender"), Terms.FUNC("foo", List("a"), Terms.REF("a")))), None)
    ).explicitGet()

    val allBytes = sc.bytes().arr
    ScriptReader.fromBytes(allBytes) shouldBe Right(sc)
  }

}
