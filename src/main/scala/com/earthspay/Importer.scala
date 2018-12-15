package com.earthspay

import java.io._

import com.google.common.primitives.Ints
import com.typesafe.config.ConfigFactory
import com.earthspay.account.{Address, AddressScheme}
import com.earthspay.block.Block
import com.earthspay.consensus.PoSSelector
import com.earthspay.db.openDB
import com.earthspay.history.{CheckpointServiceImpl, StorageFactory}
import com.earthspay.mining.MultiDimensionalMiningConstraint
import com.earthspay.settings.{EarthsSettings, loadConfig}
import com.earthspay.state.ByteStr
import com.earthspay.state.appender.BlockAppender
import com.earthspay.transaction.Transaction
import com.earthspay.utils._
import com.earthspay.utx.UtxPool
import monix.execution.Scheduler
import org.slf4j.bridge.SLF4JBridgeHandler

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

object Importer extends ScorexLogging {
  def main(args: Array[String]): Unit = {

    SLF4JBridgeHandler.removeHandlersForRootLogger()
    SLF4JBridgeHandler.install()

    val configFilename     = Try(args(0)).toOption.getOrElse("earths-testnet.conf")
    val blockchainFilename = Try(args(1))
    val importHeight       = Try(args(2)).map(_.toInt).getOrElse(Int.MaxValue)

    val config   = loadConfig(ConfigFactory.parseFile(new File(configFilename)))
    val settings = EarthsSettings.fromConfig(config)
    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = settings.blockchainSettings.addressSchemeCharacter.toByte
    }

    implicit val scheduler: Scheduler = Scheduler.singleThread("appender")
    val utxPoolStub = new UtxPool {
      override def putIfNew(tx: Transaction)                               = ???
      override def removeAll(txs: Traversable[Transaction]): Unit          = {}
      override def accountPortfolio(addr: Address)                         = ???
      override def portfolio(addr: Address)                                = ???
      override def all                                                     = ???
      override def size                                                    = ???
      override def transactionById(transactionId: ByteStr)                 = ???
      override def packUnconfirmed(rest: MultiDimensionalMiningConstraint) = ???
      override def close(): Unit                                           = {}
    }

    val time = new NTP(settings.ntpServer)
    blockchainFilename match {
      case Success(filename) =>
        log.info(s"Loading file '$filename'")

        createInputStream(filename) match {
          case Success(inputStream) =>
            val db                = openDB(settings.dataDirectory)
            val blockchainUpdater = StorageFactory(settings, db, time)
            val pos               = new PoSSelector(blockchainUpdater, settings.blockchainSettings)
            val checkpoint        = new CheckpointServiceImpl(db, settings.checkpointsSettings)
            val extAppender       = BlockAppender(checkpoint, blockchainUpdater, time, utxPoolStub, pos, settings, scheduler) _
            checkGenesis(settings, blockchainUpdater)
            val bis           = new BufferedInputStream(inputStream)
            var quit          = false
            val lenBytes      = new Array[Byte](Ints.BYTES)
            val start         = System.currentTimeMillis()
            var counter       = 0
            var blocksToSkip  = blockchainUpdater.height - 1
            val blocksToApply = importHeight - blockchainUpdater.height + 1

            println(s"Skipping $blocksToSkip blocks(s)")

            while (!quit && counter < blocksToApply) {
              val s1 = bis.read(lenBytes)
              if (s1 == Ints.BYTES) {
                val len    = Ints.fromByteArray(lenBytes)
                val buffer = new Array[Byte](len)
                val s2     = bis.read(buffer)
                if (s2 == len) {
                  if (blocksToSkip > 0) {
                    blocksToSkip -= 1
                  } else {
                    val block = Block.parseBytes(buffer).get
                    if (blockchainUpdater.lastBlockId.contains(block.reference)) {
                      Await.result(extAppender.apply(block).runAsync, Duration.Inf) match {
                        case Left(ve) =>
                          log.error(s"Error appending block: $ve")
                          quit = true
                        case _ =>
                          counter = counter + 1
                      }
                    }
                  }
                } else {
                  println(s"$s2 != expected $len")
                  quit = true
                }
              } else {
                println(s"Expecting to read ${Ints.BYTES} but got $s1 (${bis.available()})")
                quit = true
              }
            }
            bis.close()
            inputStream.close()
            val duration = System.currentTimeMillis() - start
            log.info(s"Imported $counter block(s) in ${humanReadableDuration(duration)}")
          case Failure(_) => log.error(s"Failed to open file '$filename")
        }
      case Failure(_) => log.error("Usage: Importer <config file> <blockchain file> [height]")
    }

    time.close()
  }

  def createInputStream(filename: String): Try[FileInputStream] =
    Try {
      new FileInputStream(filename)
    }

}
