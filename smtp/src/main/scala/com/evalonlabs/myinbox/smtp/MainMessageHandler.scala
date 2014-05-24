package com.evalonlabs.myinbox.smtp

import org.subethamail.smtp.{RejectException, DropConnectionException, MessageHandler, MessageContext}
import java.io.InputStream
import java.util.{Properties, Date}
import com.evalonlabs.myinbox.util.SafeConfig
import com.evalonlabs.myinbox.actor.SmtpActorSystem
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
import com.evalonlabs.myinbox.model.AliasAddr
import com.evalonlabs.myinbox.model.Reject
import com.evalonlabs.monitoring.Logging
import akka.actor.{Actor, Props}
import java.util.concurrent.{TimeUnit, CountDownLatch}
import java.util.{HashMap => JHashMap}
import java.util.concurrent.atomic.AtomicReference

class MainMessageHandler(ctx: MessageContext) extends MessageHandler with Logging {
  val state = new JHashMap[String, AtomicReference]()
  val timestamp = new Date().getTime
  val delay = SafeConfig.getMilliseconds("myinbox.server.command-delay").getOrElse(10000L)

  def from(addr: String) {
    val mctx = MessageCtxDetails(ctx)
    val lock = new CountDownLatch(1)

    logger.debug("Helo: " + mctx.helo)
    logger.debug("From: " + from)

    val fromReceive = SmtpActorSystem.system.actorOf(Props(new Actor() {
      def receive: Receive = {
        case (FromOk(ctx, from), lock) =>
          state.put("sender", new AtomicReference(from))
          lock.countDown()
        case (Reject(reason, ctx, from), lock) =>
          val mctx = MessageCtxDetails(ctx)
          val helo = mctx.helo
          val ip = mctx.ip

          lock.countDown()
          logger.warn("Reject ip: " + ip + " helo: " + helo + " from: " + from + " reason: " + reason)
          throw new DropConnectionException(reason)
        case Greylist(reason) =>
          lock.countDown()
          throw new DropConnectionException(421, reason)
        case _ => throw new Exception("Error in processing.")
      }
    }))

    SmtpActorSystem.senderCheckActor !(fromReceive, FromReq(ctx, addr.toLowerCase), lock)
    lock.await(2, TimeUnit.MINUTES)
  }


  def recipient(addr: String) {

    logger.debug("To: " + addr)

    implicit val timeout = Timeout(2 minutes)
    val future = SmtpActorSystem.recipientCheckActor ? (inet, sender.get, addr)
    Await.result(future, 2 minutes) match {
      case AliasAddr(userAddr) => recip.lazySet(userAddr)
      case GoNext() => recip.lazySet(addr)
      case Reject(reason) =>
        logger.warn("Reject recipient ip: " + ip + " to: " + addr + " reason: " + reason)
        throw new RejectException(reason)
    }
  }


  def data(data: InputStream) {
    val session = Session.getInstance(new Properties())
    val message = new MimeMessage(session, data)

    implicit val timeout = Timeout(2 minutes)
    val future = SmtpActorSystem.messageCheckActor ? Message[MimeMessage](inet, sender.get, recip.get, message.getSubject, message)
    Await.result(future, 2 minutes) match {
      case msg: Message[MimeMessage] =>
        val userFilter = SmtpActorSystem.userFilterActor ? msg
        Await.result(userFilter, 2 minutes) match {
          case msg: Message[MimeMessage] =>
            SmtpActorSystem.persistMsgActor ! msg
          case Reject(reason) =>
            logger.debug("Reject data: reason: " + reason)
            throw new RejectException(reason)
        }
      case Reject(reason) =>
        logger.debug("Reject data ip: " + ip + " reason: " + reason)
        throw new RejectException(reason)
    }
  }


  def done() {
    logger.debug("Finished at " + new Date().getTime)
  }
}
