package com.earthspay

import com.typesafe.config.ConfigFactory
import com.earthspay.account.PrivateKeyAccount
import com.earthspay.block.{Block, MicroBlock}
import com.earthspay.common.state.ByteStr
import com.earthspay.common.utils.EitherExt2
import com.earthspay.consensus.nxt.NxtLikeConsensusBlockData
import com.earthspay.crypto._
import com.earthspay.features.BlockchainFeatures
import com.earthspay.lagonaki.mocks.TestBlock
import com.earthspay.settings.{BlockchainSettings, TestFunctionalitySettings, EarthsSettings}
import com.earthspay.transaction.Transaction

package object history {
  val MaxTransactionsPerBlockDiff = 10
  val MaxBlocksInMemory           = 5
  val DefaultBaseTarget           = 1000L
  val DefaultBlockchainSettings = BlockchainSettings(
    addressSchemeCharacter = 'N',
    functionalitySettings = TestFunctionalitySettings.Enabled,
    genesisSettings = null
  )

  val config   = ConfigFactory.load()
  val settings = EarthsSettings.fromConfig(config)

  val MicroblocksActivatedAt0BlockchainSettings: BlockchainSettings = DefaultBlockchainSettings.copy(
    functionalitySettings = DefaultBlockchainSettings.functionalitySettings.copy(preActivatedFeatures = Map(BlockchainFeatures.NG.id -> 0)))

  val MicroblocksActivatedAt0EarthsSettings: EarthsSettings = settings.copy(blockchainSettings = MicroblocksActivatedAt0BlockchainSettings)

  val DefaultEarthsSettings: EarthsSettings = settings.copy(blockchainSettings = DefaultBlockchainSettings,
                                                          featuresSettings = settings.featuresSettings.copy(autoShutdownOnUnsupportedFeature = false))

  val defaultSigner       = PrivateKeyAccount(Array.fill(KeyLength)(0))
  val generationSignature = ByteStr(Array.fill(Block.GeneratorSignatureLength)(0: Byte))

  def buildBlockOfTxs(refTo: ByteStr, txs: Seq[Transaction]): Block = customBuildBlockOfTxs(refTo, txs, defaultSigner, 1, 0L)

  def customBuildBlockOfTxs(refTo: ByteStr,
                            txs: Seq[Transaction],
                            signer: PrivateKeyAccount,
                            version: Byte,
                            timestamp: Long,
                            bTarget: Long = DefaultBaseTarget): Block =
    Block
      .buildAndSign(
        version = version,
        timestamp = timestamp,
        reference = refTo,
        consensusData = NxtLikeConsensusBlockData(baseTarget = bTarget, generationSignature = generationSignature),
        transactionData = txs,
        signer = signer,
        Set.empty
      )
      .explicitGet()

  def customBuildMicroBlockOfTxs(totalRefTo: ByteStr,
                                 prevTotal: Block,
                                 txs: Seq[Transaction],
                                 signer: PrivateKeyAccount,
                                 version: Byte,
                                 ts: Long): (Block, MicroBlock) = {
    val newTotalBlock = customBuildBlockOfTxs(totalRefTo, prevTotal.transactionData ++ txs, signer, version, ts)
    val nonSigned = MicroBlock
      .buildAndSign(
        generator = signer,
        transactionData = txs,
        prevResBlockSig = prevTotal.uniqueId,
        totalResBlockSig = newTotalBlock.uniqueId
      )
      .explicitGet()
    (newTotalBlock, nonSigned)
  }

  def buildMicroBlockOfTxs(totalRefTo: ByteStr, prevTotal: Block, txs: Seq[Transaction], signer: PrivateKeyAccount): (Block, MicroBlock) = {
    val newTotalBlock = buildBlockOfTxs(totalRefTo, prevTotal.transactionData ++ txs)
    val nonSigned = MicroBlock
      .buildAndSign(
        generator = signer,
        transactionData = txs,
        prevResBlockSig = prevTotal.uniqueId,
        totalResBlockSig = newTotalBlock.uniqueId
      )
      .explicitGet()
    (newTotalBlock, nonSigned)
  }

  def randomSig: ByteStr = TestBlock.randomOfLength(Block.BlockIdLength)

  def chainBlocks(txs: Seq[Seq[Transaction]]): Seq[Block] = {
    def chainBlocksR(refTo: ByteStr, txs: Seq[Seq[Transaction]]): Seq[Block] = txs match {
      case (x :: xs) =>
        val block = buildBlockOfTxs(refTo, x)
        block +: chainBlocksR(block.uniqueId, xs)
      case _ => Seq.empty
    }

    chainBlocksR(randomSig, txs)
  }

  def chainBaseAndMicro(totalRefTo: ByteStr, base: Transaction, micros: Seq[Seq[Transaction]]): (Block, Seq[MicroBlock]) =
    chainBaseAndMicro(totalRefTo, Seq(base), micros, defaultSigner, 3, 0L)

  def chainBaseAndMicro(totalRefTo: ByteStr,
                        base: Seq[Transaction],
                        micros: Seq[Seq[Transaction]],
                        signer: PrivateKeyAccount,
                        version: Byte,
                        timestamp: Long): (Block, Seq[MicroBlock]) = {
    val block = customBuildBlockOfTxs(totalRefTo, base, signer, version, timestamp)
    val microBlocks = micros
      .foldLeft((block, Seq.empty[MicroBlock])) {
        case ((lastTotal, allMicros), txs) =>
          val (newTotal, micro) = customBuildMicroBlockOfTxs(totalRefTo, lastTotal, txs, signer, version, timestamp)
          (newTotal, allMicros :+ micro)
      }
      ._2
    (block, microBlocks)
  }

  def spoilSignature(b: Block): Block = b.copy(signerData = b.signerData.copy(signature = TestBlock.randomSignature()))
}