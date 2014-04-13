package com.evalonlabs.myinbox.actor

import akka.actor.Actor
import javax.mail.internet.MimeMessage
import com.typesafe.scalalogging.slf4j.Logging
import scala.language.postfixOps
import com.evalonlabs.myinbox.model.Message

class UserFilterActor extends Actor with Logging {

	def receive = {

		case message: Message =>
      // TODO check this message with User's preferences
      sender ! message

    case msg => logger error "Unknown message: " + msg.toString

	}

}
