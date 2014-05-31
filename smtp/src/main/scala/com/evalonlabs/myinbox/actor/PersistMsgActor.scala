package com.evalonlabs.myinbox.actor

import akka.actor.{ActorRef, Actor}
import javax.mail.internet.MimeMessage
import com.typesafe.scalalogging.slf4j.Logging
import java.util.{HashMap => JHashMap, Date}
import com.evalonlabs.myinbox.util._
import scala.language.postfixOps
import com.evalonlabs.myinbox.b2b.Sender
import com.evalonlabs.myinbox.model.{PersistMsgReq, UsrFilterReq, Message}
import org.subethamail.smtp.MessageContext
import java.util.concurrent.atomic.AtomicReference

class PersistMsgActor extends Actor with Logging {

  def receive = {

    case (receiver: ActorRef, PersistMsgReq(ctx: MessageContext, message: Message[MimeMessage]),
    state: JHashMap[String, AtomicReference[String]]) =>
      val cleanMessage = Mail.filterMultiparts(message.body)
      val sentDate = Option(message.body.getSentDate).getOrElse(new Date())
      val messageID = UUID.get()
      val uKey = User.getUKey(message.to)
      val secret = User.getEncryptionKey(uKey)
      val messageBody = Mail.toBytes(cleanMessage)
      val encrypted = Crypto.inAES64(messageBody, secret)
      val compressed = Compress.zip(encrypted)

      try {
        Inbox.add(messageID, message.from, message.to, message.subject, compressed, sentDate)
        Inbox.index(messageID, message.from, message.to, message.subject, new String(messageBody), sentDate)
        Sender.index(message.from, message.inet)

        SmtpActorSystem.userPrefsActor !(messageID, message)

        // TODO - notify next services
      }
      catch {
        case e: Exception =>
          logger.error("Exception saving message at " + new Date().getTime)
        // TODO Dead letter channel
      }

    case x => logger.error("Unknown message: " + x.toString)
  }

}
