package com.evalonlabs.myinbox.model

import java.net.InetAddress

case class Message[T](
                       inet: InetAddress,
                       from: String,
                       to: String = "",
                       subject: String = "",
                       body: T = null)

