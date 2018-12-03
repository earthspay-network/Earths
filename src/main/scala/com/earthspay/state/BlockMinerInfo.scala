package com.earthspay.state

import com.earthspay.block.Block.BlockId
import com.earthspay.consensus.nxt.NxtLikeConsensusBlockData

case class BlockMinerInfo(consensus: NxtLikeConsensusBlockData, timestamp: Long, blockId: BlockId)
