package com.evalonlabs.myinbox.smtp

import org.subethamail.smtp.server.SMTPServer
import com.evalonlabs.myinbox.util.SafeConfig
import java.net.InetAddress
import com.typesafe.scalalogging.slf4j.Logging
import java.util.Date
import org.subethamail.smtp.{MessageHandler, MessageContext, MessageHandlerFactory}


object Main extends App with Logging {

  val smtpServer = new SMTPServer(new MessageHandlerFactory {
    override def create(ctx: MessageContext): MessageHandler = new MainMessageHandler(ctx)
  })
  smtpServer.setConnectionTimeout(
    SafeConfig.getMilliseconds("myinbox.server.connection-timeout").getOrElse(30000L).toInt
  )
  smtpServer.setMaxConnections(
    SafeConfig.getInt("myinbox.server.max-connections").getOrElse(200)
  )
  smtpServer.setMaxRecipients(1)
  smtpServer.setMaxMessageSize(
    SafeConfig.getBytes("myinbox.data.max-message-size").getOrElse(100000L).toInt
  )
  smtpServer.setBindAddress(
    InetAddress.getByName(SafeConfig("myinbox.server.bind-address").getOrElse("127.0.0.1"))
  )
  smtpServer.setPort(
    SafeConfig.getInt("myinbox.server.bind-port").getOrElse(25000)
  )
  smtpServer.setSoftwareName("myInbox")

  logger info ("myInbox startup at " + new Date().getTime)
  smtpServer.start()

}
