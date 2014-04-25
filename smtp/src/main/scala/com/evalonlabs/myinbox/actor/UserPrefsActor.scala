package com.evalonlabs.myinbox.actor

import akka.actor.Actor
import javax.mail.internet.MimeMessage
import com.typesafe.scalalogging.slf4j.Logging
import scala.language.postfixOps
import com.evalonlabs.myinbox.model.Message

class UserPrefsActor extends Actor with Logging {

	def receive = {

		case (messageID: String, message: Message) =>
      // TODO apply user's preferences
      message

    case _ => logger error "Unknown message: " + _.toString

	}

}
