package com.earthspay.http

import akka.http.scaladsl.model.StatusCodes
import com.typesafe.config.ConfigFactory
import com.earthspay.http.ApiMarshallers._
import com.earthspay.settings.RestAPISettings
import com.earthspay.state.{Blockchain, Diff}
import com.earthspay.utx.UtxPool
import com.earthspay.{RequestGen, TestTime}
import io.netty.channel.group.ChannelGroup
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.concurrent.Eventually
import play.api.libs.json.Writes
import com.earthspay.account.Address
import com.earthspay.api.http.assets.{AssetsApiRoute, TransferV1Request, TransferV2Request}
import com.earthspay.transaction.Transaction
import com.earthspay.transaction.transfer._
import com.earthspay.wallet.Wallet

class AssetsRouteSpec extends RouteSpec("/assets") with RequestGen with PathMockFactory with Eventually {

  private val settings    = RestAPISettings.fromConfig(ConfigFactory.load())
  private val wallet      = stub[Wallet]
  private val utx         = stub[UtxPool]
  private val allChannels = stub[ChannelGroup]
  private val state       = stub[Blockchain]

  private val seed               = "seed".getBytes()
  private val senderPrivateKey   = Wallet.generateNewAccount(seed, 0)
  private val receiverPrivateKey = Wallet.generateNewAccount(seed, 1)

  (wallet.privateKeyAccount _).when(senderPrivateKey.toAddress).onCall((_: Address) => Right(senderPrivateKey)).anyNumberOfTimes()
  (utx.putIfNew _).when(*).onCall((_: Transaction) => Right((true, Diff.empty))).anyNumberOfTimes()
  (allChannels.writeAndFlush(_: Any)).when(*).onCall((_: Any) => null).anyNumberOfTimes()

  "/transfer" - {
    val route = AssetsApiRoute(settings, wallet, utx, allChannels, state, new TestTime()).route

    def posting[A: Writes](v: A): RouteTestResult = Post(routePath("/transfer"), v).addHeader(ApiKeyHeader) ~> route

    "accepts TransferRequest" in {
      val req = TransferV1Request(
        assetId = None,
        feeAssetId = None,
        amount = 1 * Earths,
        fee = Earths / 3,
        sender = senderPrivateKey.address,
        attachment = Some("attachment"),
        recipient = receiverPrivateKey.address,
        timestamp = Some(System.currentTimeMillis())
      )

      posting(req) ~> check {
        status shouldBe StatusCodes.OK
        responseAs[TransferTransactionV1]
      }
    }

    "accepts VersionedTransferRequest" in {
      val req = TransferV2Request(
        version = 2,
        assetId = None,
        amount = 1 * Earths,
        feeAssetId = None,
        fee = Earths / 3,
        sender = senderPrivateKey.address,
        attachment = None,
        recipient = receiverPrivateKey.address,
        timestamp = Some(System.currentTimeMillis())
      )

      posting(req) ~> check {
        status shouldBe StatusCodes.OK
        responseAs[TransferV2Request]
      }
    }

    "returns a error if it is not a transfer request" in {
      val req = issueReq.sample.get
      posting(req) ~> check {
        status shouldNot be(StatusCodes.OK)
      }
    }
  }

}
