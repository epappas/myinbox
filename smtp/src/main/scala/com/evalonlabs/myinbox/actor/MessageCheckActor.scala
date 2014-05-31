package com.evalonlabs.myinbox.actor

import akka.actor.{ActorRef, Actor}
import javax.mail.internet.MimeMessage
import com.evalonlabs.myinbox.util.SafeConfig
import com.evalonlabs.myinbox.model._
import com.typesafe.scalalogging.slf4j.Logging
import org.subethamail.smtp.MessageContext
import java.util.{HashMap => JHashMap}
import java.util.concurrent.atomic.AtomicReference
import com.evalonlabs.myinbox.model.Message

class MessageCheckActor extends Actor with Logging {

  def receive = {

    case (receiver: ActorRef, MsgCheckReq(ctx: MessageContext, message: Message[MimeMessage]),
    state: JHashMap[String, AtomicReference[String]]) =>

      message.body.getSize < SafeConfig.getBytes("myinbox.data.max-message-size").getOrElse(100000L).toInt match {
        case false => receiver ! Reject("Ivnvalid", ctx, state.get("sender").get())
        case true =>
          // TODO Flood control check
          receiver ! (MsgCheckOk(ctx, message), state)
      }

    case msg => logger.error("Unknown message: " + msg.toString)

  }

}
