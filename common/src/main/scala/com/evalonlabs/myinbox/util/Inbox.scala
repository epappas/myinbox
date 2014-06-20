package com.evalonlabs.myinbox.util

import com.evalonlabs.store.Redis
import java.util.Date

import scala.util.parsing.json.JSONObject

object Inbox extends Object with Redis {

  def add(messageID: String, sender: String, recipient: String, subject: String, gzipMsg: String, date: Date) {
    redis.hset(inboxHSKey(recipient), messageID, gzipMsg)
    redis.hset(inboxSendersHSKey(recipient), messageID, sender)
    redis.hset(inboxSubjectHSKey(recipient), messageID, subject)
  }

  def index(messageID: String, sender: String, recipient: String, subject: String, msg: String, date: Date) {
    redis.sadd(inboxSSKey(recipient), date.getTime, JSONObject(Map(
      "messageID" -> messageID,
      "subject" -> subject,
      "copydrip" -> msg.substring(0, 20)
    )))
  }

  def categorize(uKey: String, messageId: String, category: String) {
    redis.rpush(inboxCategoryLSKey(uKey), category, messageId)
  }

  def tag(uKey: String, messageId: String, tag: String) {
    redis.rpush(inboxTagLSKey(uKey), tag, messageId)
  }

  def get(recipient: String, messageID: String): String = {
    redis.hget[String](inboxHSKey(recipient), messageID).orNull
  }
}
