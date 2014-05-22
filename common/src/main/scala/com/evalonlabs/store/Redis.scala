package com.evalonlabs.store

import com.evalonlabs.myinbox.util.{Crypto, SafeConfig}
import com.redis.RedisClient
import com.redis.serialization.Parse

trait Redis {
  import Parse.Implicits._

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

  def inboxHSKey(userMail: String) = "users:".concat(Crypto.md5(userMail).toString.concat(":inbox"))

  def inboxSSKey(userMail: String) = "users:".concat(Crypto.md5(userMail).toString.concat(":inbox:list"))

  def inboxSendersHSKey(userMail: String) = "users:".concat(Crypto.md5(userMail).toString.concat(":inbox:sender"))

  def inboxSubjectHSKey(userMail: String) = "users:".concat(Crypto.md5(userMail).toString.concat(":inbox:subject"))

  def inboxTagLSKey(ukey: String) = "users:".concat(ukey.concat(":inbox:tag"))

  def inboxCategoryLSKey(ukey: String) = "users:".concat(ukey.concat(":inbox:category"))

//  def subscribe(channel: String): Any = {
//    newRedis(redisHost, redisPort).subscribe(channel)
//  }
}
