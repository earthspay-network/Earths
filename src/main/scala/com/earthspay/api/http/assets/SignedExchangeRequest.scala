package com.earthspay.api.http.assets

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}
import com.earthspay.account.PublicKeyAccount
import com.earthspay.api.http.BroadcastRequest
import com.earthspay.transaction.TransactionParsers.SignatureStringLength
import com.earthspay.transaction.ValidationError
import com.earthspay.transaction.assets.exchange.{ExchangeTransaction, Order}

object SignedExchangeRequest {
  implicit val orderFormat: Format[Order]                                 = com.earthspay.transaction.assets.exchange.OrderJson.orderFormat
  implicit val signedExchangeRequestFormat: Format[SignedExchangeRequest] = Json.format
}

case class SignedExchangeRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                 senderPublicKey: String,
                                 @ApiModelProperty(value = "Buy Order")
                                 order1: Order,
                                 @ApiModelProperty(value = "Sell Order")
                                 order2: Order,
                                 @ApiModelProperty(required = true, example = "1000000")
                                 amount: Long,
                                 @ApiModelProperty(required = true)
                                 price: Long,
                                 @ApiModelProperty(required = true)
                                 fee: Long,
                                 @ApiModelProperty(required = true)
                                 buyMatcherFee: Long,
                                 @ApiModelProperty(required = true)
                                 sellMatcherFee: Long,
                                 @ApiModelProperty(required = true)
                                 timestamp: Long,
                                 @ApiModelProperty(required = true)
                                 signature: String)
    extends BroadcastRequest {
  def toTx: Either[ValidationError, ExchangeTransaction] =
    for {
      _sender    <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
      _t         <- ExchangeTransaction.create(order1, order2, amount, price, buyMatcherFee, sellMatcherFee, fee, timestamp, _signature)
    } yield _t
}
