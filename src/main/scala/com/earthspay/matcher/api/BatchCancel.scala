package com.earthspay.matcher.api
import com.earthspay.account.Address
import com.earthspay.transaction.assets.exchange.AssetPair

case class BatchCancel(address: Address, assetPair: Option[AssetPair], timestamp: Long)
