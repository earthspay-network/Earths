package com.earthspay.it.matcher

import com.earthspay.account.PrivateKeyAccount
import com.earthspay.it.Node
import com.earthspay.transaction.assets.exchange.Order

sealed trait MatcherCommand extends Product with Serializable
object MatcherCommand {
  case class Place(node: Node, order: Order)                            extends MatcherCommand
  case class Cancel(node: Node, owner: PrivateKeyAccount, order: Order) extends MatcherCommand
}
