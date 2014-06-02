package com.evalonlabs.myinbox.http.route.call

import com.evalonlabs.net.http.{HttpResponse => MyHttpResponse, RouteHandler, ContextSystem}
import io.netty.channel.ChannelHandlerContext
import scala.collection.immutable.List
import io.netty.handler.codec.http.{DefaultFullHttpRequest, HttpContent, HttpRequest}
import io.netty.util.CharsetUtil
import java.util
import scala.util.parsing.json.JSONArray

class InboundCallHandler(system: ContextSystem) extends RouteHandler {

  override def handle(ctx: ChannelHandlerContext, path: String, params: util.Map[String, Object], request: HttpRequest, msg: HttpContent) {
    val fullReq: DefaultFullHttpRequest = request.asInstanceOf[DefaultFullHttpRequest]
    val content = fullReq.content()
    val payload = content.toString(CharsetUtil.UTF_8)
    val headersPayload = new JSONArray(List(request.headers().entries()))
    val responsePayload = "{\"status\": 200, \"headers\": \"" + headersPayload + "\", \"message\": " + payload + "}"

    println(responsePayload)

    MyHttpResponse.sendOk(ctx, responsePayload)
  }
}

object Inbound {
  def apply(system: ContextSystem): RouteHandler =
    new InboundCallHandler(system)
}
