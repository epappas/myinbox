package com.evalonlabs.myinbox.model

import org.subethamail.smtp.MessageContext

case class RecipientReq(ctx: MessageContext, from: String)
