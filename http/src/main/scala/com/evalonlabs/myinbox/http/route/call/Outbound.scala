package com.evalonlabs.myinbox.http.route.call

import java.util

import com.evalonlabs.myinbox.util.{Compress, Inbox}
import com.evalonlabs.net.http.{ContextSystem, RouteHandler, HttpResponse => MyHttpResponse}
import io.netty.channel.ChannelHandlerContext
import io.netty.handler.codec.http.{HttpContent, HttpRequest}


class OutboundCallHandler(system: ContextSystem) extends RouteHandler {

  override def handle(ctx: ChannelHandlerContext, path: String, params: util.Map[String, Object], request: HttpRequest, msg: HttpContent) {
    val messageID = params.get("mid").toString
    val mail = params.get("ukey").toString

    val payload = Inbox.get(mail, messageID) match {
      case msg: String =>
//        val uKey = User.getUKey(mail)
//        val secret = User.getEncryptionKey(uKey)
//        val salt = User.getEncryptionSalt(uKey)
        val uncompressed = Compress.unzip(msg)
//        val unencrypted = Crypto.fromAES64(uncompressed, secret, salt)
        uncompressed
      case null => ""
    }

    val responsePayload = "{\"status\": 200, \"message\": \"" + payload + "\"}"
    println(responsePayload)

    MyHttpResponse.sendOk(ctx, responsePayload)
  }
}

object Outbound {
  def apply(system: ContextSystem): RouteHandler =
    new OutboundCallHandler(system)
}
