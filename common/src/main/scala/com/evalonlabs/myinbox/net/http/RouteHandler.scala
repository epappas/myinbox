package com.evalonlabs.myinbox.net.http

import io.netty.channel.ChannelHandlerContext
import java.util

trait RouteHandler {
  def handle(ctx: ChannelHandlerContext, path: String, params: util.Map)
}
