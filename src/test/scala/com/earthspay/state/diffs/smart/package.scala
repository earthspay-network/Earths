package com.earthspay.state.diffs

import com.earthspay.features.BlockchainFeatures
import com.earthspay.settings.{FunctionalitySettings, TestFunctionalitySettings}

package object smart {
  val smartEnabledFS: FunctionalitySettings =
    TestFunctionalitySettings.Enabled.copy(
      preActivatedFeatures =
        Map(BlockchainFeatures.SmartAccounts.id -> 0, BlockchainFeatures.SmartAssets.id -> 0, BlockchainFeatures.DataTransaction.id -> 0))
}
