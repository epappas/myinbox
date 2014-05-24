package com.evalonlabs.myinbox.model

import org.subethamail.smtp.MessageContext

case class FromReq(ctx: MessageContext, from: String)
