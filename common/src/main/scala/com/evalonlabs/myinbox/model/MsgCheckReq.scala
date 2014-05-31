package com.evalonlabs.myinbox.model

import org.subethamail.smtp.MessageContext
import javax.mail.internet.MimeMessage

case class MsgCheckReq(ctx: MessageContext,
                   msg: Message[MimeMessage])
