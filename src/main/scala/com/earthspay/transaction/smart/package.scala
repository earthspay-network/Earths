package com.earthspay.transaction

import com.earthspay.lang.v1.traits.domain.Tx.ContractTransfer
import com.earthspay.transaction.assets.exchange.Order
import shapeless._

package object smart {
  object InputPoly extends Poly1 {
    implicit def caseOrd          = at[Order](o => RealTransactionWrapper.ord(o))
    implicit def caseTx           = at[Transaction](tx => RealTransactionWrapper(tx))
    implicit def contractTransfer = at[ContractTransfer](o => o)
  }
}
