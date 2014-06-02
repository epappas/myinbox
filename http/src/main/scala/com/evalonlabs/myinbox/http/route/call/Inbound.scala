package com.evalonlabs.myinbox.http.route.call

import com.evalonlabs.net.http.{HttpResponse => MyHttpResponse, RouteHandler, ContextSystem}
import io.netty.channel.ChannelHandlerContext
import java.util
import io.netty.handler.codec.http.{HttpContent, HttpRequest}


class InboundCallHandler(system: ContextSystem) extends RouteHandler {

  override def handle(ctx: ChannelHandlerContext, path: String, params: util.Map[String, Object], request: HttpRequest, msg: HttpContent) {
    MyHttpResponse.sendOk(ctx, "{\"status\": 200, \"message\": \"inbound OK\"}")
  }
}

object Inbound {
  def apply(system: ContextSystem): RouteHandler =
    new InboundCallHandler(system)
}
