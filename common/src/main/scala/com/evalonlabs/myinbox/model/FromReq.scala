package com.evalonlabs.myinbox.model

import org.subethamail.smtp.MessageContext
import akka.actor.ActorRef

case class FromReq(ctx: MessageContext,
                   from: String)
