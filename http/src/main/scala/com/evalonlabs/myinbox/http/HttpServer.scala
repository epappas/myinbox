package com.evalonlabs.myinbox.http

import com.evalonlabs.net.{CustomInitializer, NettyServer}
import com.evalonlabs.monitoring.Logging
import io.netty.channel.socket.SocketChannel
import io.netty.bootstrap.ServerBootstrap
import io.netty.channel.{ChannelOption, ChannelPipeline, ChannelHandlerContext}
import io.netty.buffer.ByteBuf
import io.netty.handler.codec.http.{HttpObjectAggregator, HttpResponseEncoder, HttpRequestDecoder}

class HttpServer(name: String, port: Integer, initializer: CustomInitializer) extends NettyServer(name, port, initializer) with Logging {

  def onStart(bootstrap: ServerBootstrap) = {
    bootstrap.option(ChannelOption.SO_BACKLOG,   Int.box(1024))
    bootstrap.option(ChannelOption.SO_REUSEADDR, Boolean.box(true))
    bootstrap.option(ChannelOption.TCP_NODELAY, Boolean.box(true))
    bootstrap.option(ChannelOption.SO_KEEPALIVE, Boolean.box(true))
  }

  def onStop(f: => Any) = {
    logger.info("Server Stoped")
  }

  def onConnectionOpened(ctx: ChannelHandlerContext) = {
    logger.debug("Connection Opened " + ctx.channel().remoteAddress())
  }

  def onConnectionClosed(ctx: ChannelHandlerContext) = {
    logger.debug("Connection Closed " + ctx.channel().remoteAddress())
  }

  def onServerError(ctx: ChannelHandlerContext, cause: Throwable) = {
    logger.debug("Server Error" + ctx.channel().remoteAddress() + cause.getMessage + cause.getStackTrace)
  }

  def onReceived(ctx: ChannelHandlerContext, buf: ByteBuf) = {

  }
}

object HttpService {
  def apply(port: Integer): HttpServer =
    new HttpServer("http", port, new CustomInitializer {
      override def apply(ch: SocketChannel): Any = {
        val pipeline: ChannelPipeline = ch.pipeline()

        pipeline.addLast("decoder", new HttpRequestDecoder())
        pipeline.addLast("http_encoder", new HttpResponseEncoder())
        pipeline.addLast("aggregator", new HttpObjectAggregator(65536))
        pipeline.addLast("handler", new HttpHandler(null))

        pipeline
      }
    })
}