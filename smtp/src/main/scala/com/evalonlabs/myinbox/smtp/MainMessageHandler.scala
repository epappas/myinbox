package com.evalonlabs.myinbox.smtp

import org.subethamail.smtp.{RejectException, DropConnectionException, MessageHandler, MessageContext}
import com.typesafe.scalalogging.slf4j.Logging
import java.io.InputStream
import java.net.InetSocketAddress
import java.util.{Properties, Date}
import com.evalonlabs.myinbox.util.SafeConfig
import com.evalonlabs.myinbox.actor.SmtpAkkaSystem
import akka.pattern.ask
import com.evalonlabs.myinbox.model._
import akka.util.Timeout
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.concurrent.Await
import javax.mail.Session
import javax.mail.internet.MimeMessage
import com.evalonlabs.myinbox.model.Greylist
import com.evalonlabs.myinbox.model.GoNext
import scala.Some
import com.evalonlabs.myinbox.model.AliasAddr
import com.evalonlabs.myinbox.model.Reject

class MainMessageHandler(ctx: MessageContext) extends MessageHandler with Logging {

  val inet = ctx.getRemoteAddress.asInstanceOf[InetSocketAddress].getAddress
  val ip = inet.getHostAddress
  var sender: Option[String] = None
  var recip: Option[String] = None
  val timestamp = new Date().getTime
  val delay = SafeConfig.getMilliseconds("myinbox.server.command-delay").getOrElse(10000L)

  def from(addr: String) {

    val helo = Option(ctx.getHelo).getOrElse(ip).toLowerCase
    val from = addr.toLowerCase

    logger debug "Helo: " + helo
    logger debug "From: " + from

    implicit val timeout = Timeout(2 minutes)
    val future = SmtpAkkaSystem.senderCheckActor ?(inet, helo, from)
    Await.result(future, 2 minutes) match {
      case GoNext() => sender = Some(from)
      case Reject(reason) =>
        logger warn ("Reject ip: " + ip + " helo: " + helo + " from: " + from + " reason: " + reason)
        SmtpAkkaSystem.metricsActor ! Blocked
        throw new DropConnectionException(reason)
      case Greylist(reason) =>
        SmtpAkkaSystem.metricsActor ! Blocked
        throw new DropConnectionException(421, reason)
      case _ => throw new Exception("Error in processing.")
    }
  }


  def recipient(addr: String) {

    logger debug "To: " + to

    implicit val timeout = Timeout(2 minutes)
    val future = SmtpAkkaSystem.recipientCheckActor ? (inet, sender.get, addr)
    Await.result(future, 2 minutes) match {
      case AliasAddr(userAddr) => recip = Some(userAddr)
      case GoNext() => recip = Some(addr)
      case Reject(reason) =>
        logger warn ("Reject recipient ip: " + ip + " to: " + addr + " reason: " + reason)
        SmtpAkkaSystem.metricsActor ! Blocked
        throw new RejectException(reason)
    }
  }


  def data(data: InputStream) {
    val session = Session.getInstance(new Properties())
    val message = new MimeMessage(session, data)

    implicit val timeout = Timeout(2 minutes)
    val future = SmtpAkkaSystem.messageCheckActor ? Message[MimeMessage](inet, sender.get, recip, message.getSubject, message)
    Await.result(future, 2 minutes) match {
      case msg: Message[MimeMessage] =>
        val userFilter = SmtpAkkaSystem.userFilterActor ? msg
        Await.result(userFilter, 2 minutes) match {
          case msg: Message[MimeMessage] =>
            SmtpAkkaSystem.persistMsgActor ! msg
          case Reject(reason) =>
            logger debug "Reject data: reason: " + reason
            throw new RejectException(reason)
        }
      case Reject(reason) =>
        logger debug "Reject data ip: " + ip + " reason: " + reason
        throw new RejectException(reason)
    }
  }


  def done() {
    logger debug "Finished at " + new Date().getTime
  }
}
