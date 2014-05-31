package com.evalonlabs.myinbox.actor

import akka.actor.{ActorRef, Actor}
import javax.mail.internet.MimeMessage
import com.typesafe.scalalogging.slf4j.Logging
import scala.language.postfixOps
import com.evalonlabs.myinbox.model.{UsrFilterOk, UsrFilterReq, RecipientReq, Message}
import org.subethamail.smtp.MessageContext
import java.util.{HashMap => JHashMap}
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.CountDownLatch

class UserFilterActor extends Actor with Logging {

  def receive = {
    case (receiver: ActorRef, UsrFilterReq(ctx: MessageContext, msg: Message[MimeMessage]),
    state: JHashMap[String, AtomicReference[String]]) =>
      // TODO check this message with User's preferences
      receiver ! (UsrFilterOk(ctx, msg), state)

    case msg => logger.error("Unknown message: " + msg.toString)

  }

}
