package com.earthspay.transaction

import com.earthspay.common.state.ByteStr
import com.earthspay.crypto
import monix.eval.Coeval

trait FastHashId extends ProvenTransaction {

  val id: Coeval[AssetId] = Coeval.evalOnce(ByteStr(crypto.fastHash(bodyBytes())))
}
