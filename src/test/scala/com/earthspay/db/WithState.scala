package com.earthspay.db

import java.nio.file.Files

import com.typesafe.config.ConfigFactory
import com.earthspay.TestHelpers
import com.earthspay.database.LevelDBWriter
import com.earthspay.history.Domain
import com.earthspay.settings.{FunctionalitySettings, EarthsSettings, loadConfig}
import com.earthspay.state.{Blockchain, BlockchainUpdaterImpl}
import com.earthspay.utils.TimeImpl

trait WithState {
  private def withState[A](fs: FunctionalitySettings)(f: Blockchain => A): A = {
    val path = Files.createTempDirectory("leveldb-test")
    val db   = openDB(path.toAbsolutePath.toString)
    try f(new LevelDBWriter(db, fs))
    finally {
      db.close()
      TestHelpers.deleteRecursively(path)
    }
  }

  def withStateAndHistory(fs: FunctionalitySettings)(test: Blockchain => Any): Unit = withState(fs)(test)

  def withDomain[A](settings: EarthsSettings = EarthsSettings.fromConfig(loadConfig(ConfigFactory.load())))(test: Domain => A): A = {
    val time = new TimeImpl

    try withState(settings.blockchainSettings.functionalitySettings) { blockchain =>
      val bcu = new BlockchainUpdaterImpl(blockchain, settings, time)
      try test(Domain(bcu))
      finally bcu.shutdown()
    } finally {
      time.close()
    }
  }
}
