package com.evalonlabs.net

import com.evalonlabs.myinbox.monitoring.Logging
import java.util.concurrent.atomic.AtomicReference
import io.netty.channel._
import io.netty.bootstrap.ServerBootstrap
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.nio.NioServerSocketChannel
import io.netty.channel.socket.SocketChannel
import io.netty.channel.group.{ChannelGroup, DefaultChannelGroup}
import io.netty.util.concurrent.GlobalEventExecutor
import io.netty.buffer.ByteBuf

abstract class NettyServer(name: String, port: Integer, initializer: CustomInitializer) extends Logging {
  private val state: AtomicReference[State.State] = new AtomicReference[State.State](State.STOPPED)
  private val maxThreads = Runtime.getRuntime availableProcessors()
  private val group: EventLoopGroup = new NioEventLoopGroup()
  private val workerGroup: EventLoopGroup = new NioEventLoopGroup(maxThreads)
  private implicit val channelGroup: ChannelGroup = new DefaultChannelGroup(name, GlobalEventExecutor.INSTANCE)

  def start() = start0()

  def stop() = stop0()

  protected def start0() = {
    if (state.compareAndSet(State.STOPPED, State.RUNNING)) {
      val bootstrap: ServerBootstrap = new ServerBootstrap()
      bootstrap.group(group, workerGroup)
        .channel(classOf[NioServerSocketChannel])
        .childHandler(new ServerInitializer(initializer))

      onStart(bootstrap)

      // bootstrap.option(ChannelOption.TCP_NODELAY, true)
      // bootstrap.option(ChannelOption.SO_KEEPALIVE, true)

      val channel = bootstrap.bind(port).channel()
      channelGroup.add(channel)
      logger.debug("Server: [" + name + "] Started on port:" + port)
    }
  }

  protected def stop0() = {
    if (state.compareAndSet(State.RUNNING, State.STOPPED)) {
      channelGroup.close().awaitUninterruptibly()
      logger.debug("Server: [" + name + "] Stopped on port:" + port)
    }
  }

  protected def onStart(bootstrap: ServerBootstrap)

  protected def onStop(f: => Any)

  protected def onConnectionOpened(ctx: ChannelHandlerContext)

  protected def onConnectionClosed(ctx: ChannelHandlerContext)

  protected def onServerError(ctx: ChannelHandlerContext, cause: Throwable)

  protected def onReceived(ctx: ChannelHandlerContext, buf: ByteBuf)

  class ServerInitializer(initializer: CustomInitializer)(implicit channelGroup: ChannelGroup) extends ChannelInitializer[SocketChannel] {

    override def initChannel(ch: SocketChannel) {
      if (this.initializer != null) {
        val pipeline: ChannelPipeline = ch.pipeline()

        pipeline.addLast("connection", new ConnectionProxy(channelGroup))

        // pipeline.addLast("decoder", new HttpRequestDecoder())
        // pipeline.addLast("http_encoder", new HttpResponseEncoder())
        // pipeline.addLast("aggregator", new HttpObjectAggregator(65536))

        this.initializer.apply(ch)
      }
    }
  }

  sealed class ConnectionProxy(channelGroup: ChannelGroup) extends ChannelInboundHandlerAdapter {

    override def channelActive(ctx: ChannelHandlerContext) {
      channelGroup.add(ctx.channel())
      onConnectionOpened(ctx)
      super.channelActive(ctx)
    }

    override def channelInactive(ctx: ChannelHandlerContext) {
      channelGroup.remove(ctx.channel())
      onConnectionClosed(ctx)
      super.channelInactive(ctx)

    }

    override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable) {
      onServerError(ctx, cause)
    }

    override def channelRead(ctx: ChannelHandlerContext, msg: Any) {
      super.channelRead(ctx, msg)

      msg match {
        case buf: ByteBuf =>
          onReceived(ctx, buf)
      }
    }
  }

}

object State extends Enumeration {
  type State = Value
  val RUNNING, STOPPED = Value
}

trait CustomInitializer {
  def apply(ch: SocketChannel): Any
}