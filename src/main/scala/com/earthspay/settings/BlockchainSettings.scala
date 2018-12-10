package com.earthspay.settings

import com.typesafe.config.Config
import com.earthspay.state.ByteStr
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.EnumerationReader._

import scala.concurrent.duration._

case class FunctionalitySettings(featureCheckBlocksPeriod: Int,
                                 blocksForFeatureActivation: Int,
                                 allowTemporaryNegativeUntil: Long,
                                 requireSortedTransactionsAfter: Long,
                                 generationBalanceDepthFrom50To1000AfterHeight: Int,
                                 minimalGeneratingBalanceAfter: Long,
                                 allowTransactionsFromFutureUntil: Long,
                                 allowUnissuedAssetsUntil: Long,
                                 allowInvalidReissueInSameBlockUntilTimestamp: Long,
                                 allowMultipleLeaseCancelTransactionUntilTimestamp: Long,
                                 resetEffectiveBalancesAtHeight: Int,
                                 blockVersion3AfterHeight: Int,
                                 preActivatedFeatures: Map[Short, Int],
                                 doubleFeaturesPeriodsAfterHeight: Int) {
  val dontRequireSortedTransactionsAfter: Int    = blockVersion3AfterHeight
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
    allowTemporaryNegativeUntil = 1544400000000L,
    requireSortedTransactionsAfter = 1544400000000L,
    generationBalanceDepthFrom50To1000AfterHeight = 0,
    minimalGeneratingBalanceAfter = 1544400000000L,
    allowTransactionsFromFutureUntil = 1544400000000L,
    allowUnissuedAssetsUntil = 1544400000000L,
    allowInvalidReissueInSameBlockUntilTimestamp = 1544400000000L,
    allowMultipleLeaseCancelTransactionUntilTimestamp = 1544400000000L,
    resetEffectiveBalancesAtHeight = 1,
    blockVersion3AfterHeight = 0,
    preActivatedFeatures = Map.empty,
    doubleFeaturesPeriodsAfterHeight = 0
  )

  val TESTNET = apply(
    featureCheckBlocksPeriod = 3000,
    blocksForFeatureActivation = 2700,
    allowTemporaryNegativeUntil = 1544400000000L,
    requireSortedTransactionsAfter = 1544400000000L,
    generationBalanceDepthFrom50To1000AfterHeight = 0,
    minimalGeneratingBalanceAfter = 0,
    allowTransactionsFromFutureUntil = 1544400000000L,
    allowUnissuedAssetsUntil = 1544400000000L,
    allowInvalidReissueInSameBlockUntilTimestamp = 1544400000000L,
    allowMultipleLeaseCancelTransactionUntilTimestamp = 1544400000000L,
    resetEffectiveBalancesAtHeight = 1,
    blockVersion3AfterHeight = 0,
    preActivatedFeatures = Map.empty,
    doubleFeaturesPeriodsAfterHeight = Int.MaxValue
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
    1544400000000L,
    1544400000000L,
    Constants.UnitsInEarth * Constants.TotalEarths,
    ByteStr.decodeBase58("y2U655NdDeJDCdQtPE24RxwvxfH3d6C3gngM9pYrqeeP19Wj4KBDBFgLj6qJqe1hdkP69VYy7BNmLt4nj7ps7d7").toOption,
    List(
      GenesisTransactionSettings("3PDbLWPXYi86s6QfF9qgYWkpyz3MeeUZzgn", Constants.UnitsInEarth * Constants.TotalEarths - 5 * Constants.UnitsInEarth),
      GenesisTransactionSettings("3P3M1yrHSwmCtVo5FSyDuUvgMRbCv4UHo3M", Constants.UnitsInEarth),
      GenesisTransactionSettings("3P2nCjK3Y1AjZ9xUbP7MXVdF1CYcM5Wzrmj", Constants.UnitsInEarth),
      GenesisTransactionSettings("3PCwpBjVSZSJu3fhaRhVhGKnrGYWg9ghb1m", Constants.UnitsInEarth),
      GenesisTransactionSettings("3P72UCJdSV73xVHjRXPLLNknsYkwbtcA7Hn", Constants.UnitsInEarth),
      GenesisTransactionSettings("3PBnt6D3hqm72dRpcEux2uHnpkWwQzbde2m", Constants.UnitsInEarth)
    ),
    153722867L,
    60.seconds
  )

  val TESTNET = GenesisSettings(
    1544400000000L,
    1544400000000L,
    Constants.UnitsInEarth * Constants.TotalEarths,
    ByteStr.decodeBase58("5iHmLkmhek2ArwcqVFktQQyUKAknTZDicFJWVtWw9nFK4bc1eQPBYHWWrN4b3sbHaVZvX9Y1ekFfw6J1WxjV4TrE").toOption,
    List(
      GenesisTransactionSettings("3MtPa4o2ExreEKwP2C3og9RX6ZmozZNokc7", (Constants.UnitsInEarth * Constants.TotalEarths * 0.04).toLong),
      GenesisTransactionSettings("3NCy7BprhjFzTKHXm2kau3Uh6hrfBoZAdYK", (Constants.UnitsInEarth * Constants.TotalEarths * 0.02).toLong),
      GenesisTransactionSettings("3N3gn1nEtR9b2J6kWbTKPR9P3dFwDTRp2Rf", (Constants.UnitsInEarth * Constants.TotalEarths * 0.02).toLong),
      GenesisTransactionSettings("3Mx6AYd5LT1xitaCtGkMTQWBnd93ANMn2B7", (Constants.UnitsInEarth * Constants.TotalEarths * 0.02).toLong),
      GenesisTransactionSettings("3MzFVj8J5D8P9p71zYa1VujyXR4vsUCbEUh",
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
