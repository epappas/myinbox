package com.evalonlabs.myinbox.actor

import akka.actor.{Props, ActorSystem}
import akka.routing.FromConfig

object SmtpActorSystem {

	val system = ActorSystem("MyInboxSystem")
	val senderCheckActor = system.actorOf(Props[SenderCheckActor])
	val recipientCheckActor = system.actorOf(Props[RecipientCheckActor])
	val messageCheckActor = system.actorOf(Props[MessageCheckActor])
  val userFilterActor = system.actorOf(Props[UserFilterActor])
	val persistMsgActor = system.actorOf(Props[PersistMsgActor])
  val userPrefsActor = system.actorOf(Props[UserPrefsActor])
}
