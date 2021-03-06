package com.evalonlabs.myinbox.http.route.call

import com.evalonlabs.net.http.{ContextSystem, RouteHandler, HttpResponse => MyHttpResponse}
import io.netty.channel.ChannelHandlerContext
import java.util
import io.netty.handler.codec.http.{HttpContent, HttpRequest}

class IndexCallHandler(system: ContextSystem) extends RouteHandler {

  override def handle(ctx: ChannelHandlerContext, path: String, params: util.Map[String, Object], request: HttpRequest, msg: HttpContent) {
    MyHttpResponse.sendOk(ctx, "{\"status\": 200, \"message\": \"OK\"}")
  }
}

object IndexCall {
  def apply(system: ContextSystem): RouteHandler =
    new IndexCallHandler(system)
}