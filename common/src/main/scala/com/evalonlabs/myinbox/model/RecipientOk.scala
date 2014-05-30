package com.evalonlabs.myinbox.model

import org.subethamail.smtp.MessageContext

case class RecipientOk(ctx: MessageContext,
                       from: String,
                       isAlias: Boolean = false)