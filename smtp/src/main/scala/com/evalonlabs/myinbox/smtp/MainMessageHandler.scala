package com.evalonlabs.myinbox.smtp

import org.subethamail.smtp.{RejectException, DropConnectionException, MessageHandler, MessageContext}
import java.io.InputStream
import java.util.{Properties, Date}
import com.evalonlabs.myinbox.util.SafeConfig
import com.evalonlabs.myinbox.actor.SmtpActorSystem
import com.evalonlabs.myinbox.model._
import scala.language.postfixOps
import javax.mail.Session
import javax.mail.internet.MimeMessage
import com.evalonlabs.myinbox.model.Greylist
import com.evalonlabs.myinbox.model.Reject
import com.evalonlabs.monitoring.Logging
import akka.actor.{Actor, Props}
import java.util.concurrent.{TimeUnit, CountDownLatch}
import java.util.{HashMap => JHashMap}
import java.util.concurrent.atomic.AtomicReference

class MainMessageHandler(ctx: MessageContext) extends MessageHandler with Logging {
  val state = new JHashMap[String, AtomicReference[String]]()
  val timestamp = new Date().getTime
  val delay = SafeConfig.getMilliseconds("myinbox.server.command-delay").getOrElse(10000L)

  state.put("sender", new AtomicReference[String]())
  state.put("recipient", new AtomicReference[String]())

  def from(addr: String) {
    val mctx = MessageCtxDetails(ctx)
    val lock = new CountDownLatch(1)

    logger.debug("Helo: " + mctx.helo)
    logger.debug("From: " + addr)

    val localReceive = SmtpActorSystem.system.actorOf(Props(new Actor() {
      override def receive: Receive = {
        case (FromOk(ctx: MessageContext, from: String),
        thatState: JHashMap[String, AtomicReference[String]],
        lock: CountDownLatch) =>
          thatState.get("sender").lazySet(from)
          lock.countDown()

        case (Reject(reason: String, ctx: MessageContext, from: String),
        thatState: JHashMap[String, AtomicReference[String]],
        lock: CountDownLatch) =>
          val mctx = MessageCtxDetails(ctx)
          val helo = mctx.helo
          val ip = mctx.ip

          lock.countDown()
          logger.warn("Reject ip: " + ip + " helo: " + helo + " from: " + from + " reason: " + reason)
          throw new DropConnectionException(reason)

        case (Greylist(reason: String, ctx: MessageContext, from: String),
        thatState: JHashMap[String, AtomicReference[String]],
        lock: CountDownLatch) =>
          lock.countDown()
          throw new DropConnectionException(421, reason)

        case _ =>
          lock.countDown()
          throw new Exception("Error in processing sender.")
      }
    }))

    SmtpActorSystem.senderCheckActor !(localReceive, FromReq(ctx, addr.toLowerCase), state, lock)
    lock.await(2, TimeUnit.MINUTES)
  }


  def recipient(addr: String) {

    val lock = new CountDownLatch(1)

    logger.debug("To: " + addr)

    val localReceive = SmtpActorSystem.system.actorOf(Props(new Actor {
      override def receive: Receive = {
        case (RecipientOk(ctx: MessageContext, addr: String, false),
        thatState: JHashMap[String, AtomicReference[String]],
        lock: CountDownLatch) =>

          thatState.get("recipient").lazySet(addr)
          lock.countDown()

        case (RecipientOk(ctx: MessageContext, addr: String, true),
        thatState: JHashMap[String, AtomicReference[String]],
        lock: CountDownLatch) =>

          thatState.get("recipient").lazySet(addr)
          lock.countDown()

        case (Reject(reason: String, ctx: MessageContext, addr: String),
        _, lock: CountDownLatch) =>

          val mctx = MessageCtxDetails(ctx)
          val ip = mctx.ip

          logger.warn("Reject recipient ip: " + ip + " to: " + addr + " reason: " + reason)
          lock.countDown()
          throw new RejectException(reason)

        case _ =>
          lock.countDown()
          throw new Exception("Error in processing recipient.")
      }
    }))

    SmtpActorSystem.recipientCheckActor !(localReceive, RecipientReq(ctx, addr), state, lock)
    lock.await(2, TimeUnit.MINUTES)
  }


  def data(data: InputStream) {
    val session = Session.getInstance(new Properties())
    val message = new MimeMessage(session, data)
    val sender:String = state.get("sender").get()
    val recip:String = state.get("recipient").get()
    val mctx = MessageCtxDetails(ctx)
    val inet = mctx.inet
    val msg = Message[MimeMessage](inet, sender, recip, message.getSubject, message)

    val localReceive = SmtpActorSystem.system.actorOf(Props(new Actor {
      override def receive: Receive = {
        case (MsgCheckOk(ctx: MessageContext, msg: Message[MimeMessage]),
        thatState: JHashMap[String, AtomicReference[String]]) =>

          SmtpActorSystem.userFilterActor !(self, UsrFilterReq(ctx, msg), thatState)

        case (UsrFilterOk(ctx: MessageContext, msg: Message[MimeMessage]),
        thatState: JHashMap[String, AtomicReference[String]]) =>

          SmtpActorSystem.persistMsgActor !(self, PersistMsgReq(ctx, msg), thatState)

        case Reject(reason: String, ctx: MessageContext, addr: String) =>
          val mctx = MessageCtxDetails(ctx)
          val ip = mctx.ip

          logger.warn("Reject data: <" + addr + "> (" + ip + ") reason: " + reason)
          throw new RejectException(reason)
      }
    }))

    SmtpActorSystem.messageCheckActor ! (localReceive, MsgCheckReq(ctx, msg), state)
  }


  def done() {
    logger.info("Finished at " + new Date().getTime)
  }
}
