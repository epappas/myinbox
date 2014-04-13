package com.evalonlabs.myinbox.actor

import akka.actor.Actor
import javax.mail.internet.MimeMessage
import com.evalonlabs.myinbox.util.SafeConfig
import com.evalonlabs.myinbox.model.{Message, GoNext, Reject}
import com.typesafe.scalalogging.slf4j.Logging

class MessageCheckActor extends Actor with Logging {

	def receive = {

		case message: Message[MimeMessage] => {

			message.body.getSize < SafeConfig.getBytes("myinbox.data.max-message-size").getOrElse(100000L).toInt match {
				case false => sender ! Reject("Ivnvalid")
				case true =>
          // TODO Flood control check
          sender ! message
      }
		}

		case msg => logger error "Unknown message: " + msg.toString

	}

}
