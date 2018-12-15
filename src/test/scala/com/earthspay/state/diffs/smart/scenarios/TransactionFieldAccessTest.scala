package com.earthspay.state.diffs.smart.scenarios

import com.earthspay.lang.v1.compiler.CompilerV1
import com.earthspay.lang.v1.parser.Parser
import com.earthspay.state.diffs.smart._
import com.earthspay.state._
import com.earthspay.state.diffs.{assertDiffAndState, assertDiffEi, produce}
import com.earthspay.utils.compilerContext
import com.earthspay.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import com.earthspay.lagonaki.mocks.TestBlock
import com.earthspay.lang.ScriptVersion.Versions.V1
import com.earthspay.transaction.GenesisTransaction
import com.earthspay.transaction.lease.LeaseTransaction
import com.earthspay.transaction.smart.SetScriptTransaction
import com.earthspay.transaction.transfer._

class TransactionFieldAccessTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  private def preconditionsTransferAndLease(
      code: String): Gen[(GenesisTransaction, SetScriptTransaction, LeaseTransaction, TransferTransactionV2)] = {
    val untyped = Parser(code).get.value
    val typed   = CompilerV1(compilerContext(V1, isAssetScript = false), untyped).explicitGet()._1
    preconditionsTransferAndLease(typed)
  }

  private val script =
    """
      |
      | match tx {
      | case ttx: TransferTransaction =>
      |       isDefined(ttx.assetId)==false
      |   case other =>
      |       false
      | }
      """.stripMargin

  property("accessing field of transaction without checking its type first results on exception") {
    forAll(preconditionsTransferAndLease(script)) {
      case ((genesis, script, lease, transfer)) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(transfer)), smartEnabledFS) { case _ => () }
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(lease)), smartEnabledFS)(totalDiffEi =>
          totalDiffEi should produce("TransactionNotAllowedByScript"))
    }
  }
}
