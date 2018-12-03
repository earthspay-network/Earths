package com.earthspay

import com.earthspay.utils.base58Length
import com.earthspay.block.{Block, MicroBlock}

package object transaction {

  type AssetId = com.earthspay.state.ByteStr
  val AssetIdLength: Int       = com.earthspay.crypto.DigestSize
  val AssetIdStringLength: Int = base58Length(AssetIdLength)
  type DiscardedTransactions = Seq[Transaction]
  type DiscardedBlocks       = Seq[Block]
  type DiscardedMicroBlocks  = Seq[MicroBlock]
  type AuthorizedTransaction = Authorized with Transaction
}
