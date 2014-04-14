package com.evalonlabs.myinbox.actor

import akka.actor.{Props, ActorSystem}
import akka.routing.FromConfig

object SmtpAkkaSystem {

	val system = ActorSystem("MyInboxSystem")
	val senderCheckActor = system.actorOf(Props[SenderCheckActor].withRouter(FromConfig()), "sender")
	val recipientCheckActor = system.actorOf(Props[RecipientCheckActor].withRouter(FromConfig()), "recipient")
	val messageCheckActor = system.actorOf(Props[MessageCheckActor].withRouter(FromConfig()), "msg")
  val userFilterActor = system.actorOf(Props[UserFilterActor].withRouter(FromConfig()), "userfilter")
	val persistMsgActor = system.actorOf(Props[PersistMsgActor].withRouter(FromConfig()), "save")
  val userPrefsActor = system.actorOf(Props[UserPrefsActor].withRouter(FromConfig()), "userprefs")
}
