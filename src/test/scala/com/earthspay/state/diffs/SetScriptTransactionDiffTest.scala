package com.earthspay.state.diffs

import com.earthspay.common.utils.EitherExt2
import com.earthspay.features.BlockchainFeatures
import com.earthspay.lagonaki.mocks.TestBlock
import com.earthspay.lang.StdLibVersion
import com.earthspay.lang.StdLibVersion.V1
import com.earthspay.lang.contract.Contract
import com.earthspay.lang.contract.Contract.{CallableAnnotation, CallableFunction}
import com.earthspay.lang.v1.FunctionHeader.Native
import com.earthspay.lang.v1.compiler.Terms
import com.earthspay.lang.v1.compiler.Terms._
import com.earthspay.settings.TestFunctionalitySettings
import com.earthspay.transaction.GenesisTransaction
import com.earthspay.transaction.smart.SetScriptTransaction
import com.earthspay.transaction.smart.script.ContractScript
import com.earthspay.transaction.smart.script.v1.ExprScript
import com.earthspay.{NoShrink, TransactionGen, WithDB}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class SetScriptTransactionDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDB {

  private val fs = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Map(BlockchainFeatures.SmartAccounts.id -> 0, BlockchainFeatures.Ride4DApps.id -> 0))

  val preconditionsAndSetScript: Gen[(GenesisTransaction, SetScriptTransaction)] = for {
    version <- Gen.oneOf(SetScriptTransaction.supportedVersions.toSeq)
    master  <- accountGen
    ts      <- timestampGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
    fee    <- smallFeeGen
    script <- Gen.option(scriptGen)
  } yield (genesis, SetScriptTransaction.selfSigned(master, script, fee, ts).explicitGet())

  property("setting script results in account state") {
    forAll(preconditionsAndSetContract) {
      case (genesis, setScript) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(setScript)), fs) {
          case (blockDiff, newState) =>
            newState.accountScript(setScript.sender) shouldBe setScript.script
        }
    }
  }

  val preconditionsAndSetContract: Gen[(GenesisTransaction, SetScriptTransaction)] = for {
    version <- Gen.oneOf(SetScriptTransaction.supportedVersions.toSeq)
    master  <- accountGen
    ts      <- timestampGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
    fee <- smallFeeGen
    script = ContractScript(
      StdLibVersion.V3,
      Contract(
        List.empty,
        List(CallableFunction(CallableAnnotation("sender"), Terms.FUNC("foo", List("a"), FUNCTION_CALL(Native(203), List(REF("a"), REF("sender")))))),
        None
      )
    )
  } yield (genesis, SetScriptTransaction.selfSigned(master, script.toOption, fee, ts).explicitGet())

  property("setting contract results in account state") {
    forAll(preconditionsAndSetContract) {
      case (genesis, setScript) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(setScript)), fs) {
          case (blockDiff, newState) =>
            newState.accountScript(setScript.sender) shouldBe setScript.script
        }
    }
  }

  property("Script with BlockV2 only works after Ride4DApps feature activation") {
    import com.earthspay.lagonaki.mocks.TestBlock.{create => block}

    val settingsUnactivated = TestFunctionalitySettings.Enabled.copy(
      preActivatedFeatures = Map(
        BlockchainFeatures.Ride4DApps.id -> 3
      ))
    val settingsActivated = TestFunctionalitySettings.Enabled.copy(
      preActivatedFeatures = Map(
        BlockchainFeatures.Ride4DApps.id -> 0
      ))
    val setup = for {
      master <- accountGen
      ts     <- positiveLongGen
      genesis = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      expr    = BLOCK(LET("x", CONST_LONG(3)), CONST_BOOLEAN(true))
      script  = ExprScript(V1, expr, checkSize = false).explicitGet()
      tx      = SetScriptTransaction.selfSigned(master, Some(script), 100000, ts + 1).explicitGet()
    } yield (genesis, tx)

    forAll(setup) {
      case (genesis, tx) =>
        assertDiffEi(Seq(block(Seq(genesis))), block(Seq(tx)), settingsUnactivated) { blockDiffEi =>
          blockDiffEi should produce("Ride4DApps has not been activated yet")
        }

        assertDiffEi(Seq(block(Seq(genesis))), block(Seq(tx)), settingsActivated) { blockDiffEi =>
          blockDiffEi shouldBe 'right
        }
    }
  }
}
