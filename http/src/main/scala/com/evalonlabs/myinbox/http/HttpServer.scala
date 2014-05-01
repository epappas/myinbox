package com.evalonlabs.myinbox.http

import com.evalonlabs.myinbox.net.NettyServer
import com.evalonlabs.myinbox.monitoring.Logging
import io.netty.channel.socket.SocketChannel
import io.netty.bootstrap.ServerBootstrap
import io.netty.channel.{ChannelPipeline, ChannelHandlerContext}
import io.netty.buffer.ByteBuf
import io.netty.handler.codec.http.{HttpObjectAggregator, HttpResponseEncoder, HttpRequestDecoder}

class HttpServer(name: String, port: Integer)(initializer: (SocketChannel) => Any) extends NettyServer(name, port)(initializer) with Logging {

  def onStart(bootstrap: ServerBootstrap) = {

  }

  def onStop(f: => Any) = {

  }

  def onConnectionOpened(ctx: ChannelHandlerContext) = {

  }

  def onConnectionClosed(ctx: ChannelHandlerContext) = {

  }

  def onServerError(ctx: ChannelHandlerContext, cause: Throwable) = {

  }

  def onReceived(ctx: ChannelHandlerContext, buf: ByteBuf) = {

  }
}

object HttpService {
  def apply(port: Integer): HttpServer =
    new HttpServer("http", port)((ch: SocketChannel) => {
      val pipeline: ChannelPipeline = ch.pipeline()

      pipeline.addLast("decoder", new HttpRequestDecoder())
      pipeline.addLast("http_encoder", new HttpResponseEncoder())
      pipeline.addLast("aggregator", new HttpObjectAggregator(65536))
      pipeline.addLast("handler", new HttpHandler(null))

      pipeline
    })
}