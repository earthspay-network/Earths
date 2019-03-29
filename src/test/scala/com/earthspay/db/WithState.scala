package com.earthspay.db

import java.nio.file.Files

import com.typesafe.config.ConfigFactory
import com.earthspay.account.Address
import com.earthspay.database.LevelDBWriter
import com.earthspay.history.Domain
import com.earthspay.settings.{FunctionalitySettings, EarthsSettings, loadConfig}
import com.earthspay.state.{Blockchain, BlockchainUpdaterImpl}
import com.earthspay.transaction.AssetId
import com.earthspay.utils.Implicits.SubjectOps
import com.earthspay.{NTPTime, TestHelpers}
import monix.reactive.subjects.Subject
import org.scalatest.Suite

trait WithState extends DBCacheSettings {
  protected val ignoreSpendableBalanceChanged: Subject[(Address, Option[AssetId]), (Address, Option[AssetId])] = Subject.empty
  protected def withState[A](fs: FunctionalitySettings)(f: Blockchain => A): A = {
    val path = Files.createTempDirectory("leveldb-test")
    val db   = openDB(path.toAbsolutePath.toString)
    try f(new LevelDBWriter(db, ignoreSpendableBalanceChanged, fs, maxCacheSize, 2000, 120 * 60 * 1000))
    finally {
      db.close()
      TestHelpers.deleteRecursively(path)
    }
  }

  def withStateAndHistory(fs: FunctionalitySettings)(test: Blockchain => Any): Unit = withState(fs)(test)
}

trait WithDomain extends WithState with NTPTime {
  _: Suite =>

  def withDomain[A](settings: EarthsSettings = EarthsSettings.fromConfig(loadConfig(ConfigFactory.load())))(test: Domain => A): A = {
    try withState(settings.blockchainSettings.functionalitySettings) { blockchain =>
      val bcu = new BlockchainUpdaterImpl(blockchain, ignoreSpendableBalanceChanged, settings, ntpTime)
      try test(Domain(bcu))
      finally bcu.shutdown()
    } finally {}
  }
}
