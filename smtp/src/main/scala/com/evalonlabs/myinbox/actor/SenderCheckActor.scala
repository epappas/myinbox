package com.evalonlabs.myinbox.actor

import akka.actor.{ActorRef, Actor}
import com.typesafe.scalalogging.slf4j.Logging
import java.util.{HashMap => JHashMap}
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.CountDownLatch
import org.subethamail.smtp.MessageContext
import com.evalonlabs.myinbox.model.FromReq
import com.evalonlabs.myinbox.model.FromOk

class SenderCheckActor extends Actor with Logging {

  def receive = {
    case (receiver: ActorRef, FromReq(ctx: MessageContext, from: String),
    state: JHashMap[String, AtomicReference[String]], lock: CountDownLatch) =>
      // val mctx = MessageCtxDetails(ctx) // <-- holds all the info that is needed
      // TODO Flood control check
      // TODO IPFloodFilter.check(inet)
      // TODO DNSBLFilter.check(inet)
      // TODO SPFFilter.check(inet, helo, from)
      // TODO SenderbaseFilter.check(inet)
      // TODO GreylistFilter.check(inet, helo)

      receiver !(FromOk(ctx, from), state, lock)
    // receiver ! (Greylist(reason, ctx, from), state, lock)
    // receiver ! (Reject(reason, ctx, from), state, lock)
    case _ =>
      logger.error("Unknown message: ")
  }

}
