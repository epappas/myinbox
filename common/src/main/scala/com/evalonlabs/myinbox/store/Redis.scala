package com.evalonlabs.myinbox.store

import com.evalonlabs.myinbox.util.SafeConfig
import com.redis.RedisClient

trait Redis {

  private val redisHost = SafeConfig("myinbox.redis.host").getOrElse("localhost")
  private val redisPort = SafeConfig.getInt("myinbox.redis.port").getOrElse(6379)

  val redis: RedisClient = newRedis(redisHost, redisPort)

  def newRedis(): RedisClient = {
    new RedisClient(redisHost, redisPort)
  }

  def newRedis(host: String, port: Int): RedisClient = {
    new RedisClient(host, port)
  }

  def publish(channel: String, message: String): Option[Long] = {
    redis.publish(channel, message)
  }

//  def subscribe(channel: String): Any = {
//    newRedis(redisHost, redisPort).subscribe(channel)
//  }
}
