package com.evalonlabs.net.http

import io.netty.channel.ChannelHandlerContext
import java.util
import io.netty.handler.codec.http.{HttpContent, HttpRequest}

trait RouteHandler {
  def handle(ctx: ChannelHandlerContext, path: String, params: util.Map[String, Object], request: HttpRequest, msg: HttpContent)
}
