package com.evalonlabs.myinbox.model

import org.subethamail.smtp.MessageContext

case class Greylist(reason: String, ctx: MessageContext,
                    from: String)
