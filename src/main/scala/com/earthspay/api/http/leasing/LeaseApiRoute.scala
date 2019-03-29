package com.earthspay.api.http.leasing

import akka.http.scaladsl.server.Route
import com.earthspay.account.Address
import com.earthspay.api.http._
import com.earthspay.http.BroadcastRoute
import com.earthspay.settings.RestAPISettings
import com.earthspay.state.Blockchain
import com.earthspay.transaction._
import com.earthspay.transaction.lease.{LeaseTransaction, LeaseTransactionV1}
import com.earthspay.common.utils.EitherExt2
import com.earthspay.utils.Time
import com.earthspay.utx.UtxPool
import com.earthspay.wallet.Wallet
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import javax.ws.rs.Path
import play.api.libs.json.JsNumber

@Path("/leasing")
@Api(value = "/leasing")
case class LeaseApiRoute(settings: RestAPISettings, wallet: Wallet, blockchain: Blockchain, utx: UtxPool, allChannels: ChannelGroup, time: Time)
    extends ApiRoute
    with BroadcastRoute {

  override val route = pathPrefix("leasing") {
    lease ~ cancel ~ active
  }

  def lease: Route = processRequest("lease", (t: LeaseV1Request) => doBroadcast(TransactionFactory.leaseV1(t, wallet, time)))

  def cancel: Route = processRequest("cancel", (t: LeaseCancelV1Request) => doBroadcast(TransactionFactory.leaseCancelV1(t, wallet, time)))

  @Path("/active/{address}")
  @ApiOperation(value = "Get all active leases for an address", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Wallet address ", required = true, dataType = "string", paramType = "path")
    ))
  def active: Route = (pathPrefix("active") & get) {
    pathPrefix(Segment) { address =>
      complete(Address.fromString(address) match {
        case Left(e) => ApiError.fromValidationError(e)
        case Right(a) =>
          blockchain
            .addressTransactions(a, Set(LeaseTransactionV1.typeId), Int.MaxValue, None)
            .explicitGet()
            .collect {
              case (h, lt: LeaseTransaction) if blockchain.leaseDetails(lt.id()).exists(_.isActive) =>
                lt.json() + ("height" -> JsNumber(h))
            }
      })
    }
  }
}