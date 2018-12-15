package com.earthspay.it.matcher

import com.typesafe.config.Config
import com.earthspay.it._
import com.earthspay.it.transactions.NodesFromDocker
import org.scalatest._
import com.earthspay.it.util._
import scala.concurrent.ExecutionContext

abstract class MatcherSuiteBase
    extends FreeSpec
    with Matchers
    with CancelAfterFailure
    with ReportingTestName
    with NodesFromDocker
    with BeforeAndAfterAll
    with MatcherNode {

  protected implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  val defaultAssetQuantity = 999999999999L

  val smartFee         = 0.004.earths
  val minFee           = 0.001.earths + smartFee
  val issueFee         = 1.earths + smartFee
  val leasingFee       = 0.002.earths + smartFee
  val tradeFee         = 0.003.earths
  val smartTradeFee    = tradeFee + smartFee
  val twoSmartTradeFee = tradeFee + 2 * smartFee

  protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .withDefault(4)
      .buildNonConflicting()

}
