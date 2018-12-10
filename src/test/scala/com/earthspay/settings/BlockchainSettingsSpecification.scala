package com.earthspay.settings

import com.typesafe.config.ConfigFactory
import com.earthspay.state.ByteStr
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
        |        require-sorted-transactions-after = 3
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
        |      }
        |      genesis {
        |        timestamp = 1544400000000
        |        block-timestamp = 1544400000000
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
    settings.functionalitySettings.requireSortedTransactionsAfter should be(3)
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
    settings.genesisSettings.blockTimestamp should be(1544400000000L)
    settings.genesisSettings.timestamp should be(1544400000000L)
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
    settings.functionalitySettings.allowTemporaryNegativeUntil should be(1544400000000L)
    settings.functionalitySettings.requireSortedTransactionsAfter should be(1544400000000L)
    settings.functionalitySettings.generationBalanceDepthFrom50To1000AfterHeight should be(0)
    settings.functionalitySettings.minimalGeneratingBalanceAfter should be(0)
    settings.functionalitySettings.allowTransactionsFromFutureUntil should be(1544400000000L)
    settings.functionalitySettings.allowUnissuedAssetsUntil should be(1544400000000L)
    settings.functionalitySettings.allowInvalidReissueInSameBlockUntilTimestamp should be(1544400000000L)
    settings.functionalitySettings.allowMultipleLeaseCancelTransactionUntilTimestamp should be(1544400000000L)
    settings.functionalitySettings.resetEffectiveBalancesAtHeight should be(1)
    settings.functionalitySettings.blockVersion3AfterHeight should be(0)
    settings.genesisSettings.blockTimestamp should be(1544400000000L)
    settings.genesisSettings.timestamp should be(1544400000000L)
    settings.genesisSettings.signature should be(
      ByteStr.decodeBase58("5iHmLkmhek2ArwcqVFktQQyUKAknTZDicFJWVtWw9nFK4bc1eQPBYHWWrN4b3sbHaVZvX9Y1ekFfw6J1WxjV4TrE").toOption)
    settings.genesisSettings.initialBalance should be(10000000000000000L)

    settings.genesisSettings.transactions should be(
      Seq(
        GenesisTransactionSettings("3MtPa4o2ExreEKwP2C3og9RX6ZmozZNokc7", 400000000000000L),
        GenesisTransactionSettings("3NCy7BprhjFzTKHXm2kau3Uh6hrfBoZAdYK", 200000000000000L),
        GenesisTransactionSettings("3N3gn1nEtR9b2J6kWbTKPR9P3dFwDTRp2Rf", 200000000000000L),
        GenesisTransactionSettings("3Mx6AYd5LT1xitaCtGkMTQWBnd93ANMn2B7", 200000000000000L),
        GenesisTransactionSettings("3MzFVj8J5D8P9p71zYa1VujyXR4vsUCbEUh", 9000000000000000L)
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
    settings.functionalitySettings.allowTemporaryNegativeUntil should be(1544400000000L)
    settings.functionalitySettings.requireSortedTransactionsAfter should be(1544400000000L)
    settings.functionalitySettings.generationBalanceDepthFrom50To1000AfterHeight should be(0L)
    settings.functionalitySettings.minimalGeneratingBalanceAfter should be(1544400000000L)
    settings.functionalitySettings.allowTransactionsFromFutureUntil should be(1544400000000L)
    settings.functionalitySettings.allowUnissuedAssetsUntil should be(1544400000000L)
    settings.functionalitySettings.allowInvalidReissueInSameBlockUntilTimestamp should be(1544400000000L)
    settings.functionalitySettings.allowMultipleLeaseCancelTransactionUntilTimestamp should be(1544400000000L)
    settings.functionalitySettings.resetEffectiveBalancesAtHeight should be(1)
    settings.genesisSettings.blockTimestamp should be(1544400000000L)
    settings.genesisSettings.timestamp should be(1544400000000L)
    settings.genesisSettings.signature should be(
      ByteStr.decodeBase58("y2U655NdDeJDCdQtPE24RxwvxfH3d6C3gngM9pYrqeeP19Wj4KBDBFgLj6qJqe1hdkP69VYy7BNmLt4nj7ps7d7").toOption)
    settings.genesisSettings.initialBalance should be(9223372036800000000L)
    settings.genesisSettings.transactions should be(
      Seq(
        GenesisTransactionSettings("3PDbLWPXYi86s6QfF9qgYWkpyz3MeeUZzgn", 9223372036300000000L),
        GenesisTransactionSettings("3P3M1yrHSwmCtVo5FSyDuUvgMRbCv4UHo3M", 100000000L),
        GenesisTransactionSettings("3P2nCjK3Y1AjZ9xUbP7MXVdF1CYcM5Wzrmj", 100000000L),
        GenesisTransactionSettings("3PCwpBjVSZSJu3fhaRhVhGKnrGYWg9ghb1m", 100000000L),
        GenesisTransactionSettings("3P72UCJdSV73xVHjRXPLLNknsYkwbtcA7Hn", 100000000L),
        GenesisTransactionSettings("3PBnt6D3hqm72dRpcEux2uHnpkWwQzbde2m", 100000000L)
      ))
  }
}
