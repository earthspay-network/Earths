package com.earthspay.lang.v1.traits.domain

import com.earthspay.common.state.ByteStr

case class APair(amountAsset: Option[ByteStr], priceAsset: Option[ByteStr])
