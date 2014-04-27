package com.evalonlabs.myinbox.net.http

import io.netty.channel.ChannelHandlerContext

trait RouteHandler {
  def handle(ctx: ChannelHandlerContext, path: String, params: Map)
}
