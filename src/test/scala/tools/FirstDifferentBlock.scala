package tools

import play.api.libs.json.Json

object FirstDifferentBlock extends App {

  def get(url: String) = scala.io.Source.fromURL(url).mkString

  def blockAt(nodeHttp: String, blockHeight: Int)    = get(nodeHttp + "/blocks/at/" + blockHeight)
  def blockSigAt(nodeHttp: String, blockHeight: Int) = (Json.parse(blockAt(nodeHttp, blockHeight)) \ "signature").get.as[String]

  def nodeComparator(node1: String, node2: String)(h: Int): Boolean = {
    blockSigAt(node1, h) == blockSigAt(node2, h)
  }

  val TESTNET1 = "http://52.210.14.4:7779"
  val TESTNET2 = "http://52.210.14.4:7719"
  val TESTNET3 = "http://52.210.14.4:7729"
  val TESTNET4 = "http://52.210.14.4:7739"

  val DEVNET1  = "http://52.210.14.4:7779"
  val DEVNET1D = "http://52.210.14.4:17779"
  val DEVNET2  = "http://52.210.14.4:7719"
  val DEVNET2D = "http://52.210.14.4:17729"
  val DEVNET3  = "http://52.210.14.4:7739"
  val DEVNET3D = "http://52.210.14.4:17749"

  val MAINNET1 = "http://52.210.14.4"
  val MAINNET2 = "http://52.210.14.5"
  val MAINNET3 = "http://52.210.14.6" // 626195

  def firstDifferent(min: Int, max: Int, areSame: Int => Boolean): Int = {
    println("searching [" + min + ", " + max + ")")
    if (max - min <= 1)
      max
    else {
      val split = (min + max) / 2
      if (areSame(split))
        firstDifferent(split, max, areSame)
      else firstDifferent(min, split, areSame)
    }
  }

  println("first different block height is " + firstDifferent(1, 258, nodeComparator(DEVNET3D, DEVNET3)))
}
