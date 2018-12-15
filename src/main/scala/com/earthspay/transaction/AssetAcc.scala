package com.earthspay.transaction

import com.earthspay.account.Address

case class AssetAcc(account: Address, assetId: Option[AssetId])
