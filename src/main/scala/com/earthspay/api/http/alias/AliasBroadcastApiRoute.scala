package com.earthspay.api.http.alias

import akka.http.scaladsl.server.Route
import com.earthspay.api.http._
import com.earthspay.http.BroadcastRoute
import com.earthspay.settings.RestAPISettings
import com.earthspay.utx.UtxPool
import io.netty.channel.group.ChannelGroup

case class AliasBroadcastApiRoute(settings: RestAPISettings, utx: UtxPool, allChannels: ChannelGroup) extends ApiRoute with BroadcastRoute {
  override val route = pathPrefix("alias" / "broadcast") {
    signedCreate
  }

  def signedCreate: Route = (path("create") & post) {
    json[SignedCreateAliasV1Request] { aliasReq =>
      doBroadcast(aliasReq.toTx)
    }
  }
}
