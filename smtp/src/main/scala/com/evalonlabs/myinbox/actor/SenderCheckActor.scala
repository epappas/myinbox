package com.evalonlabs.myinbox.actor

import akka.actor.Actor
import com.evalonlabs.myinbox.model.{Message, GoNext}
import com.typesafe.scalalogging.slf4j.Logging
import java.net.InetAddress

class SenderCheckActor extends Actor with Logging {

	def receive = {

		case (inet: InetAddress, helo: String, from: String) => {
			// TODO Flood control check
      // TODO IPFloodFilter.check(inet)
      // TODO DNSBLFilter.check(inet)
      // TODO SPFFilter.check(inet, helo, from)
      // TODO SenderbaseFilter.check(inet)
      // TODO GreylistFilter.check(inet, helo)

      sender ! Message(inet, from)
		}

		case x => {
			logger error "Unknown message: " + x.toString
			sender ! GoNext()
		}

	}

}
