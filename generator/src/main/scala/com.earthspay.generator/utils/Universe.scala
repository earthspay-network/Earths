package com.earthspay.generator.utils

import com.earthspay.account.PrivateKeyAccount
import com.earthspay.state.ByteStr

object Universe {
  var AccountsWithBalances: List[(PrivateKeyAccount, Long)] = Nil
  var IssuedAssets: List[ByteStr]                           = Nil
  var Leases: List[ByteStr]                                 = Nil
}
