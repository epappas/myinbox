package com.evalonlabs.myinbox

import com.evalonlabs.myinbox.store.Redis

object Main extends App with Redis {
  redis.set("helloWorldKey", "Hello World!")

  println(redis.get("helloWorldKey"))
}
