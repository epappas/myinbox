package com.evalonlabs.myinbox.actor

import akka.actor.Actor
import com.typesafe.scalalogging.slf4j.Logging
import com.evalonlabs.myinbox.model.AliasAddr
import com.evalonlabs.myinbox.util.User
import java.net.InetAddress

class RecipientCheckActor extends Actor with Logging {

	def receive = {

		case (inet: InetAddress, from: String, addr: String) => {
			//TODO check address
      // TODO if not exists, check alias
      // TODO reject in non existence
      sender ! AliasAddr(User.addressFromAlias(addr))
		}

		case x => logger error "Unknown message: " + x.toString
	}

}
