package com.evalonlabs.myinbox.model

import org.subethamail.smtp.MessageContext
import java.net.InetSocketAddress

case class MessageCtxDetails(ctx: MessageContext) {
  val inet = ctx.getRemoteAddress.asInstanceOf[InetSocketAddress].getAddress
  val ip = inet.getHostAddress
  val helo = Option(ctx.getHelo).getOrElse(ip).toLowerCase
}
