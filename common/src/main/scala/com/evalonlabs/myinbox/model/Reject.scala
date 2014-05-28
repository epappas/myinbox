package com.evalonlabs.myinbox.model

import org.subethamail.smtp.MessageContext
import java.util.{HashMap => JHashMap}
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.CountDownLatch

case class Reject(reason: String, ctx: MessageContext,
                  from: String,
                  state: JHashMap[String, AtomicReference],
                  lock: CountDownLatch)
