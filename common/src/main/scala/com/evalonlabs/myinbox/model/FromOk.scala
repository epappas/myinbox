package com.evalonlabs.myinbox.model

import org.subethamail.smtp.MessageContext

case class FromOk(ctx: MessageContext, from: String)
