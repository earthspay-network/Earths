package com.earthspay.transaction

import com.earthspay.crypto
import com.earthspay.state.ByteStr
import monix.eval.Coeval

trait FastHashId extends ProvenTransaction {

  val id: Coeval[AssetId] = Coeval.evalOnce(ByteStr(crypto.fastHash(bodyBytes())))
}
