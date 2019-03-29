package com.earthspay.settings

import com.typesafe.config.Config
import com.earthspay.common.state.ByteStr
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.EnumerationReader._

import scala.concurrent.duration._

case class FunctionalitySettings(featureCheckBlocksPeriod: Int,
                                 blocksForFeatureActivation: Int,
                                 allowTemporaryNegativeUntil: Long,
                                 generationBalanceDepthFrom50To1000AfterHeight: Int,
                                 minimalGeneratingBalanceAfter: Long,
                                 allowTransactionsFromFutureUntil: Long,
                                 allowUnissuedAssetsUntil: Long,
                                 allowInvalidReissueInSameBlockUntilTimestamp: Long,
                                 allowMultipleLeaseCancelTransactionUntilTimestamp: Long,
                                 resetEffectiveBalancesAtHeight: Int,
                                 blockVersion3AfterHeight: Int,
                                 preActivatedFeatures: Map[Short, Int],
                                 doubleFeaturesPeriodsAfterHeight: Int,
                                 maxTransactionTimeBackOffset: FiniteDuration,
                                 maxTransactionTimeForwardOffset: FiniteDuration) {
  val allowLeasedBalanceTransferUntilHeight: Int = blockVersion3AfterHeight

  require(featureCheckBlocksPeriod > 0, "featureCheckBlocksPeriod must be greater than 0")
  require(
    (blocksForFeatureActivation > 0) && (blocksForFeatureActivation <= featureCheckBlocksPeriod),
    s"blocksForFeatureActivation must be in range 1 to $featureCheckBlocksPeriod"
  )

  def activationWindowSize(height: Int): Int =
    featureCheckBlocksPeriod * (if (height <= doubleFeaturesPeriodsAfterHeight) 1 else 2)

  def activationWindow(height: Int): Range =
    if (height < 1) Range(0, 0)
    else {
      val ws = activationWindowSize(height)
      Range.inclusive((height - 1) / ws * ws + 1, ((height - 1) / ws + 1) * ws)
    }

  def blocksForFeatureActivation(height: Int): Int =
    blocksForFeatureActivation * (if (height <= doubleFeaturesPeriodsAfterHeight) 1 else 2)

  def generatingBalanceDepth(height: Int): Int =
    if (height >= generationBalanceDepthFrom50To1000AfterHeight) 1000 else 50
}

object FunctionalitySettings {
  val MAINNET = apply(
    featureCheckBlocksPeriod = 5000,
    blocksForFeatureActivation = 4000,
    allowTemporaryNegativeUntil = 1553817600000L,
    generationBalanceDepthFrom50To1000AfterHeight = 0,
    minimalGeneratingBalanceAfter = 1553817600000L,
    allowTransactionsFromFutureUntil = 1553817600000L,
    allowUnissuedAssetsUntil = 1553817600000L,
    allowInvalidReissueInSameBlockUntilTimestamp = 1553817600000L,
    allowMultipleLeaseCancelTransactionUntilTimestamp = 1553817600000L,
    resetEffectiveBalancesAtHeight = 1,
    blockVersion3AfterHeight = 0,
    preActivatedFeatures = Map.empty,
    doubleFeaturesPeriodsAfterHeight = 0,
    maxTransactionTimeBackOffset = 120.minutes,
    maxTransactionTimeForwardOffset = 90.minutes
  )

  val TESTNET = apply(
    featureCheckBlocksPeriod = 3000,
    blocksForFeatureActivation = 2700,
    allowTemporaryNegativeUntil = 1553817600000L,
    generationBalanceDepthFrom50To1000AfterHeight = 0,
    minimalGeneratingBalanceAfter = 0,
    allowTransactionsFromFutureUntil = 1553817600000L,
    allowUnissuedAssetsUntil = 1553817600000L,
    allowInvalidReissueInSameBlockUntilTimestamp = 1553817600000L,
    allowMultipleLeaseCancelTransactionUntilTimestamp = 1553817600000L,
    resetEffectiveBalancesAtHeight = 1,
    blockVersion3AfterHeight = 0,
    preActivatedFeatures = Map.empty,
    doubleFeaturesPeriodsAfterHeight = Int.MaxValue,
    maxTransactionTimeBackOffset = 120.minutes,
    maxTransactionTimeForwardOffset = 90.minutes
  )

  val configPath = "earths.blockchain.custom.functionality"
}

case class GenesisTransactionSettings(recipient: String, amount: Long)

case class GenesisSettings(blockTimestamp: Long,
                           timestamp: Long,
                           initialBalance: Long,
                           signature: Option[ByteStr],
                           transactions: Seq[GenesisTransactionSettings],
                           initialBaseTarget: Long,
                           averageBlockDelay: FiniteDuration)

object GenesisSettings {
  val MAINNET = GenesisSettings(
    1553817600000L,
    1553817600000L,
    Constants.UnitsInEarth * Constants.TotalEarths,
    ByteStr.decodeBase58("1XqEgLSqbvekihtWuNUxWRVuiQaCnLcZsZMWa2TGCobgW74Fo9cWnkwexEeTsQX7W9qYWBDWa6ksp2nEhdZZK9s").toOption,
    List(
      GenesisTransactionSettings("3P6fbpPzXfstWKuCJrwoN1jDWywMCX8ytTi", Constants.UnitsInEarth * Constants.TotalEarths - 5 * Constants.UnitsInEarth),
      GenesisTransactionSettings("3PMjBxXRsvPmh6JfHj9BDvgPKFY8SdH5ZFR", Constants.UnitsInEarth),
      GenesisTransactionSettings("3PPsDQL3g53cQR8a6kMgKux4Tk8sToYjydY", Constants.UnitsInEarth),
      GenesisTransactionSettings("3PBuy5CXgvqVL5knGeuggvf1Ux2sBNKV9Fc", Constants.UnitsInEarth),
      GenesisTransactionSettings("3P6J7TWPcLQqzdN2RLkH7G7D34RvVGMKqzX", Constants.UnitsInEarth),
      GenesisTransactionSettings("3P5vJvyeKBks4bYtZ1DgGdo7JBZE2D1Un9z", Constants.UnitsInEarth)
    ),
    153722867L,
    60.seconds
  )

  val TESTNET = GenesisSettings(
    1553817600000L,
    1553817600000L,
    Constants.UnitsInEarth * Constants.TotalEarths,
    ByteStr.decodeBase58("3qetTQgDnpDTeqnEFmhhCtoQFAN3jho3nRFwsfN9b2MuevYvBRkbFpf7UwZRx4G9nY8XmiJbah2AWr2SbApKUJvc").toOption,
    List(
      GenesisTransactionSettings("3Ms5p8vPyZx7poSGt93fxHj8AuGrb9n2X1A", (Constants.UnitsInEarth * Constants.TotalEarths * 0.04).toLong),
      GenesisTransactionSettings("3MxhQo5YhfwhQghjs8McE8xbi9kT9oGrTyZ", (Constants.UnitsInEarth * Constants.TotalEarths * 0.02).toLong),
      GenesisTransactionSettings("3NBftXy7kGCcxFzuPtQ6LSB7EBcHmsEbQhW", (Constants.UnitsInEarth * Constants.TotalEarths * 0.02).toLong),
      GenesisTransactionSettings("3N6pRMncJ3x7CzfcHjtSQbKHXCUEkbiKCXA", (Constants.UnitsInEarth * Constants.TotalEarths * 0.02).toLong),
      GenesisTransactionSettings("3N5Jys7aibuTpknSGjYAYmtdVqPjZpK4xY9",
                                 (Constants.UnitsInEarth * Constants.TotalEarths - Constants.UnitsInEarth * Constants.TotalEarths * 0.1).toLong)
    ),
    153722867L,
    60.seconds
  )
}

case class BlockchainSettings(addressSchemeCharacter: Char, functionalitySettings: FunctionalitySettings, genesisSettings: GenesisSettings)

object BlockchainType extends Enumeration {
  val TESTNET = Value("TESTNET")
  val MAINNET = Value("MAINNET")
  val CUSTOM  = Value("CUSTOM")
}

object BlockchainSettings {
  val configPath: String = "earths.blockchain"

  def fromConfig(config: Config): BlockchainSettings = {
    val blockchainType = config.as[BlockchainType.Value](s"$configPath.type")
    val (addressSchemeCharacter, functionalitySettings, genesisSettings) = blockchainType match {
      case BlockchainType.TESTNET =>
        ('T', FunctionalitySettings.TESTNET, GenesisSettings.TESTNET)
      case BlockchainType.MAINNET =>
        ('W', FunctionalitySettings.MAINNET, GenesisSettings.MAINNET)
      case BlockchainType.CUSTOM =>
        val addressSchemeCharacter = config.as[String](s"$configPath.custom.address-scheme-character").charAt(0)
        val functionalitySettings  = config.as[FunctionalitySettings]("earths.blockchain.custom.functionality")
        val genesisSettings        = config.as[GenesisSettings]("earths.blockchain.custom.genesis")
        (addressSchemeCharacter, functionalitySettings, genesisSettings)
    }

    BlockchainSettings(
      addressSchemeCharacter = addressSchemeCharacter,
      functionalitySettings = functionalitySettings,
      genesisSettings = genesisSettings
    )
  }
}
