package com.evalonlabs.myinbox.http

import io.netty.channel.{ChannelHandlerContext, SimpleChannelInboundHandler}
import io.netty.handler.codec.http._
import com.evalonlabs.myinbox.http.route.Routes
import com.evalonlabs.net.http.HttpResponse
import java.util
import com.evalonlabs.monitoring.Logging
import com.evalonlabs.net.http.ContextSystem

class HttpHandler(system: ContextSystem) extends SimpleChannelInboundHandler[AnyRef] with Logging {

  val normalizedParams = new util.HashMap[String, Object]()
  var request: HttpRequest = null
  var decoder: QueryStringDecoder = null

  override def channelRead0(ctx: ChannelHandlerContext, msg: AnyRef) {
    msg match {
      case msg: HttpRequest =>
        this.request = msg
        if (HttpHeaders.is100ContinueExpected(this.request)) {
          send100(ctx)
        }
        this.decoder = new QueryStringDecoder(this.request.getUri)

        val params: util.Map[String, util.List[String]] = this.decoder.parameters

        if (!params.isEmpty) {
          val it = params.entrySet().iterator()
          while (it.hasNext) {
            val entry = it.next()
            this.normalizedParams.put(entry.getKey, entry.getValue.get(0))
          }
        }
        try {
          handleReq(ctx, this.decoder.path, this.request, null)
        }
        catch {
          case e: Exception =>
            e.printStackTrace()
            HttpResponse.sendError(ctx, "{\"status\":\"500\", \"message\": \"" + e + "\"}")
        }
      case msg: LastHttpContent =>
        try {
          handleReq(ctx, this.decoder.path, this.request, msg)
        }
        catch {
          case e: Exception =>
            e.printStackTrace()
            HttpResponse.sendError(ctx, "{\"status\":\"500\", \"message\": \"" + e + "\"}")
        }
    }
  }

  private def handleReq(ctx: ChannelHandlerContext, path: String, request: HttpRequest, msg: HttpContent) = request.getMethod.name match {
    case "GET" => Routes.get(system, path).handle(ctx, path, normalizedParams, this.request, null)
    case "POST" => Routes.post(system, path).handle(ctx, path, normalizedParams, this.request, msg)
    case "PUT"=> Routes.put(system, path).handle(ctx, path, normalizedParams, this.request, msg)
    case "OPTIONS" => Routes.options(system, path).handle(ctx, path, normalizedParams, this.request, null)
    case "DELETE" => Routes.delete(system, path).handle(ctx, path, normalizedParams, this.request, msg)
    case "PATCH" => Routes.patch(system, path).handle(ctx, path, normalizedParams, this.request, msg)
    case _ => HttpResponse.sendError(ctx, "{}")
  }

  private def send100(ctx: ChannelHandlerContext) {
    ctx.write(new DefaultFullHttpResponse(HttpVersion.HTTP_1_1, HttpResponseStatus.CONTINUE))
  }

  override def channelReadComplete(ctx: ChannelHandlerContext) {
    ctx.flush()
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable) {
    cause.printStackTrace()
    ctx.close()
  }
}

