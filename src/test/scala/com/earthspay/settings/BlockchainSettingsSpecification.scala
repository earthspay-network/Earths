package com.earthspay.settings

import com.typesafe.config.ConfigFactory
import com.earthspay.common.state.ByteStr
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class BlockchainSettingsSpecification extends FlatSpec with Matchers {
  "BlockchainSettings" should "read custom values" in {
    val config   = loadConfig(ConfigFactory.parseString("""earths {
        |  directory = "/earths"
        |  data-directory = "/earths/data"
        |  blockchain {
        |    type = CUSTOM
        |    custom {
        |      address-scheme-character = "C"
        |      functionality {
        |        feature-check-blocks-period = 10000
        |        blocks-for-feature-activation = 9000
        |        allow-temporary-negative-until = 1
        |        generation-balance-depth-from-50-to-1000-after-height = 4
        |        minimal-generating-balance-after = 5
        |        allow-transactions-from-future-until = 6
        |        allow-unissued-assets-until = 7
        |        allow-invalid-reissue-in-same-block-until-timestamp = 12
        |        allow-multiple-lease-cancel-transaction-until-timestamp = 14
        |        reset-effective-balances-at-height = 15
        |        block-version-3-after-height = 18
        |        pre-activated-features {
        |          19 = 100
        |          20 = 200
        |        }
        |        double-features-periods-after-height = 21
        |        max-transaction-time-back-offset = 55s
        |        max-transaction-time-forward-offset = 12d
        |      }
        |      genesis {
        |        timestamp = 1553817600000
        |        block-timestamp = 1553817600000
        |        signature = "BASE58BLKSGNATURE"
        |        initial-balance = 100000000000000
        |        initial-base-target = 153722867
        |        average-block-delay = 60s
        |        transactions = [
        |          {recipient = "BASE58ADDRESS1", amount = 50000000000001},
        |          {recipient = "BASE58ADDRESS2", amount = 49999999999999}
        |        ]
        |      }
        |    }
        |  }
        |}""".stripMargin))
    val settings = BlockchainSettings.fromConfig(config)

    settings.addressSchemeCharacter should be('C')
    settings.functionalitySettings.featureCheckBlocksPeriod should be(10000)
    settings.functionalitySettings.blocksForFeatureActivation should be(9000)
    settings.functionalitySettings.allowTemporaryNegativeUntil should be(1)
    settings.functionalitySettings.generationBalanceDepthFrom50To1000AfterHeight should be(4)
    settings.functionalitySettings.minimalGeneratingBalanceAfter should be(5)
    settings.functionalitySettings.allowTransactionsFromFutureUntil should be(6)
    settings.functionalitySettings.allowUnissuedAssetsUntil should be(7)
    settings.functionalitySettings.allowInvalidReissueInSameBlockUntilTimestamp should be(12)
    settings.functionalitySettings.allowMultipleLeaseCancelTransactionUntilTimestamp should be(14)
    settings.functionalitySettings.resetEffectiveBalancesAtHeight should be(15)
    settings.functionalitySettings.blockVersion3AfterHeight should be(18)
    settings.functionalitySettings.preActivatedFeatures should be(Map(19 -> 100, 20 -> 200))
    settings.functionalitySettings.doubleFeaturesPeriodsAfterHeight should be(21)
    settings.functionalitySettings.maxTransactionTimeBackOffset should be(55.seconds)
    settings.functionalitySettings.maxTransactionTimeForwardOffset should be(12.days)
    settings.genesisSettings.blockTimestamp should be(1553817600000L)
    settings.genesisSettings.timestamp should be(1553817600000L)
    settings.genesisSettings.signature should be(ByteStr.decodeBase58("BASE58BLKSGNATURE").toOption)
    settings.genesisSettings.initialBalance should be(100000000000000L)
    settings.genesisSettings.initialBaseTarget should be(153722867)
    settings.genesisSettings.averageBlockDelay should be(60.seconds)
    settings.genesisSettings.transactions should be(
      Seq(GenesisTransactionSettings("BASE58ADDRESS1", 50000000000001L), GenesisTransactionSettings("BASE58ADDRESS2", 49999999999999L)))
  }

  it should "read testnet settings" in {
    val config   = loadConfig(ConfigFactory.parseString("""earths {
        |  directory = "/earths"
        |  data-directory = "/earths/data"
        |  blockchain {
        |    type = TESTNET
        |  }
        |}""".stripMargin))
    val settings = BlockchainSettings.fromConfig(config)

    settings.addressSchemeCharacter should be('T')
    settings.functionalitySettings.allowTemporaryNegativeUntil should be(1553817600000L)
    settings.functionalitySettings.generationBalanceDepthFrom50To1000AfterHeight should be(0)
    settings.functionalitySettings.minimalGeneratingBalanceAfter should be(0)
    settings.functionalitySettings.allowTransactionsFromFutureUntil should be(1553817600000L)
    settings.functionalitySettings.allowUnissuedAssetsUntil should be(1553817600000L)
    settings.functionalitySettings.allowInvalidReissueInSameBlockUntilTimestamp should be(1553817600000L)
    settings.functionalitySettings.allowMultipleLeaseCancelTransactionUntilTimestamp should be(1553817600000L)
    settings.functionalitySettings.resetEffectiveBalancesAtHeight should be(1)
    settings.functionalitySettings.blockVersion3AfterHeight should be(0)
    settings.functionalitySettings.maxTransactionTimeBackOffset should be(120.minutes)
    settings.functionalitySettings.maxTransactionTimeForwardOffset should be(90.minutes)
    settings.genesisSettings.blockTimestamp should be(1553817600000L)
    settings.genesisSettings.timestamp should be(1553817600000L)
    settings.genesisSettings.signature should be(
      ByteStr.decodeBase58("3qetTQgDnpDTeqnEFmhhCtoQFAN3jho3nRFwsfN9b2MuevYvBRkbFpf7UwZRx4G9nY8XmiJbah2AWr2SbApKUJvc").toOption)
    settings.genesisSettings.initialBalance should be(9223300000000000000L)

    settings.genesisSettings.transactions should be(
      Seq(
        GenesisTransactionSettings("3Ms5p8vPyZx7poSGt93fxHj8AuGrb9n2X1A", 368932000000000000L),
        GenesisTransactionSettings("3MxhQo5YhfwhQghjs8McE8xbi9kT9oGrTyZ", 184466000000000000L),
        GenesisTransactionSettings("3NBftXy7kGCcxFzuPtQ6LSB7EBcHmsEbQhW", 184466000000000000L),
        GenesisTransactionSettings("3N6pRMncJ3x7CzfcHjtSQbKHXCUEkbiKCXA", 184466000000000000L),
        GenesisTransactionSettings("3N5Jys7aibuTpknSGjYAYmtdVqPjZpK4xY9", 8300970000000000000L)
      ))
  }

  it should "read mainnet settings" in {
    val config   = loadConfig(ConfigFactory.parseString("""earths {
        |  directory = "/earths"
        |  data-directory = "/earths/data"
        |  blockchain {
        |    type = MAINNET
        |  }
        |}""".stripMargin))
    val settings = BlockchainSettings.fromConfig(config)

    settings.addressSchemeCharacter should be('W')
    settings.functionalitySettings.allowTemporaryNegativeUntil should be(1553817600000L)
    settings.functionalitySettings.generationBalanceDepthFrom50To1000AfterHeight should be(0L)
    settings.functionalitySettings.minimalGeneratingBalanceAfter should be(1553817600000L)
    settings.functionalitySettings.allowTransactionsFromFutureUntil should be(1553817600000L)
    settings.functionalitySettings.allowUnissuedAssetsUntil should be(1553817600000L)
    settings.functionalitySettings.allowInvalidReissueInSameBlockUntilTimestamp should be(1553817600000L)
    settings.functionalitySettings.allowMultipleLeaseCancelTransactionUntilTimestamp should be(1553817600000L)
    settings.functionalitySettings.resetEffectiveBalancesAtHeight should be(1)
    settings.functionalitySettings.maxTransactionTimeBackOffset should be(120.minutes)
    settings.functionalitySettings.maxTransactionTimeForwardOffset should be(90.minutes)
    settings.genesisSettings.blockTimestamp should be(1553817600000L)
    settings.genesisSettings.timestamp should be(1553817600000L)
    settings.genesisSettings.signature should be(
      ByteStr.decodeBase58("1XqEgLSqbvekihtWuNUxWRVuiQaCnLcZsZMWa2TGCobgW74Fo9cWnkwexEeTsQX7W9qYWBDWa6ksp2nEhdZZK9s").toOption)
    settings.genesisSettings.initialBalance should be(9223300000000000000L)
    settings.genesisSettings.transactions should be(
      Seq(
        GenesisTransactionSettings("3P6fbpPzXfstWKuCJrwoN1jDWywMCX8ytTi", 9223299999500000000L),
        GenesisTransactionSettings("3PMjBxXRsvPmh6JfHj9BDvgPKFY8SdH5ZFR", 100000000L),
        GenesisTransactionSettings("3PPsDQL3g53cQR8a6kMgKux4Tk8sToYjydY", 100000000L),
        GenesisTransactionSettings("3PBuy5CXgvqVL5knGeuggvf1Ux2sBNKV9Fc", 100000000L),
        GenesisTransactionSettings("3P6J7TWPcLQqzdN2RLkH7G7D34RvVGMKqzX", 100000000L),
        GenesisTransactionSettings("3P5vJvyeKBks4bYtZ1DgGdo7JBZE2D1Un9z", 100000000L)
      ))
  }
}
