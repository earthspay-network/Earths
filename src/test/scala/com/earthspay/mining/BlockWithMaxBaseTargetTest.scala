package com.earthspay.mining

import java.security.Permission
import java.util.concurrent.{Semaphore, TimeUnit}

import com.typesafe.config.ConfigFactory
import com.earthspay.account.{Address, PrivateKeyAccount}
import com.earthspay.block.Block
import com.earthspay.common.state.ByteStr
import com.earthspay.common.utils.EitherExt2
import com.earthspay.consensus.PoSSelector
import com.earthspay.database.LevelDBWriter
import com.earthspay.db.DBCacheSettings
import com.earthspay.features.BlockchainFeatures
import com.earthspay.lagonaki.mocks.TestBlock
import com.earthspay.mining.BlockWithMaxBaseTargetTest.Env
import com.earthspay.settings.{EarthsSettings, _}
import com.earthspay.state._
import com.earthspay.state.appender.BlockAppender
import com.earthspay.state.diffs.ENOUGH_AMT
import com.earthspay.transaction.{AssetId, BlockchainUpdater, GenesisTransaction, Transaction}
import com.earthspay.utils.BaseTargetReachedMaximum
import com.earthspay.utx.UtxPool
import com.earthspay.wallet.Wallet
import com.earthspay.{TransactionGen, WithDB}
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.execution.Scheduler
import monix.execution.schedulers.SchedulerService
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{FreeSpec, Matchers, PrivateMethodTester}

import scala.concurrent.Await
import scala.concurrent.duration._

class BlockWithMaxBaseTargetTest extends FreeSpec with Matchers with WithDB with TransactionGen with PrivateMethodTester with DBCacheSettings {

  "base target limit" - {
    "node should stop if base target greater than maximum in block creation " in {
      withEnv {
        case Env(settings, pos, bcu, utxPoolStub, scheduler, account, lastBlock) =>
          var stopReasonCode = 0

          val allChannels = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
          val wallet      = Wallet(WalletSettings(None, Some("123"), None))
          val miner =
            new MinerImpl(allChannels, bcu, settings, ntpTime, utxPoolStub, wallet, pos, scheduler, scheduler)

          val signal = new Semaphore(1)
          signal.acquire()

          System.setSecurityManager(new SecurityManager {
            override def checkPermission(perm: Permission): Unit = {}

            override def checkPermission(perm: Permission, context: Object): Unit = {}

            override def checkExit(status: Int): Unit = signal.synchronized {
              super.checkExit(status)
              stopReasonCode = status
              if (status == BaseTargetReachedMaximum.code)
                signal.release()
              throw new SecurityException("System exit is not allowed")
            }
          })

          val forgeBlock = PrivateMethod[MinerImpl]('forgeBlock)
          miner invokePrivate forgeBlock(account)

          signal.tryAcquire(10, TimeUnit.SECONDS)

          stopReasonCode shouldBe BaseTargetReachedMaximum.code

          System.setSecurityManager(null)
      }
    }

    "node should stop if base target greater than maximum in block append" in {
      withEnv {
        case Env(settings, pos, bcu, utxPoolStub, scheduler, _, lastBlock) =>
          var stopReasonCode = 0

          val signal = new Semaphore(1)
          signal.acquire()

          System.setSecurityManager(new SecurityManager {
            override def checkPermission(perm: Permission): Unit = {}

            override def checkPermission(perm: Permission, context: Object): Unit = {}

            override def checkExit(status: Int): Unit = signal.synchronized {
              super.checkExit(status)
              stopReasonCode = status
              if (status == BaseTargetReachedMaximum.code)
                signal.release()
              throw new SecurityException("System exit is not allowed")
            }
          })

          val blockAppendTask = BlockAppender(bcu, ntpTime, utxPoolStub, pos, settings, scheduler)(lastBlock)
          Await.result(blockAppendTask.runAsync(scheduler), Duration.Inf)

          signal.tryAcquire(10, TimeUnit.SECONDS)

          stopReasonCode shouldBe BaseTargetReachedMaximum.code

          System.setSecurityManager(null)
      }
    }
  }

  def withEnv(f: Env => Unit): Unit = {
    val defaultWriter = new LevelDBWriter(db, ignoreSpendableBalanceChanged, TestFunctionalitySettings.Stub, maxCacheSize, 2000, 120 * 60 * 1000)

    val settings0     = EarthsSettings.fromConfig(loadConfig(ConfigFactory.load()))
    val minerSettings = settings0.minerSettings.copy(quorum = 0)
    val blockchainSettings0 = settings0.blockchainSettings.copy(
      functionalitySettings = settings0.blockchainSettings.functionalitySettings.copy(
        preActivatedFeatures = Map(BlockchainFeatures.FairPoS.id -> 1)
      )
    )
    val synchronizationSettings0 = settings0.synchronizationSettings.copy(maxBaseTargetOpt = Some(1L))
    val settings = settings0.copy(
      blockchainSettings = blockchainSettings0,
      minerSettings = minerSettings,
      synchronizationSettings = synchronizationSettings0,
      featuresSettings = settings0.featuresSettings.copy(autoShutdownOnUnsupportedFeature = false)
    )

    val bcu = new BlockchainUpdaterImpl(defaultWriter, ignoreSpendableBalanceChanged, settings, ntpTime)
    val pos = new PoSSelector(bcu, settings.blockchainSettings, settings.synchronizationSettings)

    val utxPoolStub = new UtxPool {
      override def putIfNew(tx: Transaction)                                       = ???
      override def removeAll(txs: Traversable[Transaction]): Unit                  = {}
      override def spendableBalance(addr: Address, assetId: Option[AssetId]): Long = ???
      override def pessimisticPortfolio(addr: Address): Portfolio                  = ???
      override def all                                                             = ???
      override def size                                                            = ???
      override def transactionById(transactionId: ByteStr)                         = ???
      override def packUnconfirmed(rest: MultiDimensionalMiningConstraint)         = ???
      override def close(): Unit                                                   = {}
    }
    val schedulerService: SchedulerService = Scheduler.singleThread("appender")

    try {

      val ts = ntpTime.correctedTime() - 60000
      val (account, firstBlock, secondBlock) =
        Gen
          .containerOfN[Array, Byte](32, Arbitrary.arbitrary[Byte])
          .map(PrivateKeyAccount.apply)
          .map { account =>
            val tx           = GenesisTransaction.create(account, ENOUGH_AMT, ts + 1).explicitGet()
            val genesisBlock = TestBlock.create(ts + 2, List(tx))
            val secondBlock = TestBlock.create(
              ts + 3,
              genesisBlock.uniqueId,
              Seq.empty,
              account
            )
            (account, genesisBlock, secondBlock)
          }
          .sample
          .get

      bcu.processBlock(firstBlock).explicitGet()

      f(Env(settings, pos, bcu, utxPoolStub, schedulerService, account, secondBlock))

      bcu.shutdown()
    } finally {
      bcu.shutdown()
      db.close()
    }
  }
}

object BlockWithMaxBaseTargetTest {

  final case class Env(settings: EarthsSettings,
                       pos: PoSSelector,
                       bcu: BlockchainUpdater with NG,
                       utxPool: UtxPool,
                       schedulerService: SchedulerService,
                       miner: PrivateKeyAccount,
                       lastBlock: Block)
}
