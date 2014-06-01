package com.evalonlabs.myinbox.actor

import akka.actor.{ActorRef, Actor}
import com.typesafe.scalalogging.slf4j.Logging
import com.evalonlabs.myinbox.model.{RecipientOk, RecipientReq}
import org.subethamail.smtp.MessageContext
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.CountDownLatch
import java.util.{HashMap => JHashMap}

class RecipientCheckActor extends Actor with Logging {

  def receive = {

    case (receiver: ActorRef, RecipientReq(ctx: MessageContext, from: String),
    state: JHashMap[String, AtomicReference[String]], lock: CountDownLatch) =>
      //TODO check address
      // TODO if not exists, check alias
      // TODO reject in non existence
      receiver ! (RecipientOk(ctx, from, Boolean.box(true)), state, lock )
//      sender ! AliasAddr(User.addressFromAlias(addr))

    case x => logger.error("Unknown message: " + x.toString)
  }

}
