package com.evalonlabs.myinbox.model

import org.subethamail.smtp.MessageContext

case class Reject(reason: String, ctx: MessageContext,
                  from: String)
