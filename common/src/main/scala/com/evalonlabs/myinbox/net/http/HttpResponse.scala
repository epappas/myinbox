package com.evalonlabs.myinbox.net.http

import io.netty.channel.{ChannelFutureListener, ChannelHandlerContext}
import io.netty.handler.codec.http._
import io.netty.buffer.Unpooled
import io.netty.util.CharsetUtil

object HttpResponse {
  def sendOk(ctx: ChannelHandlerContext, content: String) {
    writeResponse(ctx, HttpResponseStatus.OK, content)
  }

  def sendNotFound(ctx: ChannelHandlerContext, content: String) {
    writeResponse(ctx, HttpResponseStatus.NOT_FOUND, content)
  }

  def sendError(ctx: ChannelHandlerContext, content: String) {
    writeResponse(ctx, HttpResponseStatus.INTERNAL_SERVER_ERROR, content)
  }

  def writeResponse(ctx: ChannelHandlerContext, status: HttpResponseStatus, content: String) {
    val response: FullHttpResponse = new DefaultFullHttpResponse(
      HttpVersion.HTTP_1_1, status,
      Unpooled.copiedBuffer(content, CharsetUtil.UTF_8)
    )

    response.headers().set(HttpHeaders.Names.CONTENT_TYPE, "application/json; charset=UTF-8")

    // ignore keep-alive connections
    ctx.write(response).addListener(ChannelFutureListener.CLOSE)
  }
}