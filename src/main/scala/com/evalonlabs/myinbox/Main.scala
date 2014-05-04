package com.evalonlabs.myinbox

import com.evalonlabs.store.Redis

object Main extends App with Redis {
  redis.set("helloWorldKey", "Hello World!")

  println(redis.get("helloWorldKey"))
}
