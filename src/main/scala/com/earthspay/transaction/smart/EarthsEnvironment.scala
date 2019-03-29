package com.earthspay.transaction.smart

import com.earthspay.account.AddressOrAlias
import com.earthspay.common.state.ByteStr
import com.earthspay.common.utils.EitherExt2
import com.earthspay.lang.v1.traits._
import com.earthspay.lang.v1.traits.domain.Recipient._
import com.earthspay.lang.v1.traits.domain.Tx.ContractTransfer
import com.earthspay.lang.v1.traits.domain.{Recipient, Tx}
import com.earthspay.state._
import com.earthspay.transaction.Transaction
import com.earthspay.transaction.assets.exchange.Order
import monix.eval.Coeval
import shapeless._

object EarthsEnvironment {
  type In = Transaction :+: Order :+: ContractTransfer :+: CNil
}

class EarthsEnvironment(nByte: Byte, in: Coeval[EarthsEnvironment.In], h: Coeval[Int], blockchain: Blockchain) extends Environment {
  override def height: Long = h()

  override def inputEntity: Environment.InputEntity = {
    in.apply()
      .map(InputPoly)
  }

  override def transactionById(id: Array[Byte]): Option[Tx] =
    blockchain
      .transactionInfo(ByteStr(id))
      .map(_._2)
      .map(RealTransactionWrapper(_))

  override def data(recipient: Recipient, key: String, dataType: DataType): Option[Any] = {
    for {
      address <- recipient match {
        case Address(bytes) =>
          com.earthspay.account.Address
            .fromBytes(bytes.arr)
            .toOption
        case Alias(name) =>
          com.earthspay.account.Alias
            .buildWithCurrentChainId(name)
            .flatMap(blockchain.resolveAlias)
            .toOption
      }
      data <- blockchain
        .accountData(address, key)
        .map((_, dataType))
        .flatMap {
          case (IntegerDataEntry(_, value), DataType.Long)     => Some(value)
          case (BooleanDataEntry(_, value), DataType.Boolean)  => Some(value)
          case (BinaryDataEntry(_, value), DataType.ByteArray) => Some(ByteStr(value.arr))
          case (StringDataEntry(_, value), DataType.String)    => Some(value)
          case _                                               => None
        }
    } yield data
  }
  override def resolveAlias(name: String): Either[String, Recipient.Address] =
    blockchain
      .resolveAlias(com.earthspay.account.Alias.buildWithCurrentChainId(name).explicitGet())
      .left
      .map(_.toString)
      .right
      .map(a => Recipient.Address(ByteStr(a.bytes.arr)))

  override def chainId: Byte = nByte

  override def accountBalanceOf(addressOrAlias: Recipient, maybeAssetId: Option[Array[Byte]]): Either[String, Long] = {
    (for {
      aoa <- addressOrAlias match {
        case Address(bytes) => AddressOrAlias.fromBytes(bytes.arr, position = 0).map(_._1)
        case Alias(name)    => com.earthspay.account.Alias.buildWithCurrentChainId(name)
      }
      address <- blockchain.resolveAlias(aoa)
      balance = blockchain.balance(address, maybeAssetId.map(ByteStr(_)))
    } yield balance).left.map(_.toString)
  }
  override def transactionHeightById(id: Array[Byte]): Option[Long] =
    blockchain.transactionHeight(ByteStr(id)).map(_.toLong)
}
