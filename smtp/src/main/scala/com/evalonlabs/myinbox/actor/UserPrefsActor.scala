package com.evalonlabs.myinbox.actor

import akka.actor.Actor
import javax.mail.internet.MimeMessage
import com.typesafe.scalalogging.slf4j.Logging
import scala.language.postfixOps
import com.evalonlabs.myinbox.model.Message

class UserPrefsActor extends Actor with Logging {

	def receive = {

		case (messageID: String, message: Message[Any]) =>
      logger.trace("Check User Pref " + messageID + " to " + message.to + " " + message.from)
      // TODO apply user's preferences
      message

	}

}
