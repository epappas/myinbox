package com.evalonlabs.myinbox.http

import com.evalonlabs.monitoring.Logging

object Main extends App with Logging {
  implicit val loader: ClassLoader = Thread.currentThread().getContextClassLoader
  HttpService(8089).start()
  logger.info("Server is running at 8089")
}
