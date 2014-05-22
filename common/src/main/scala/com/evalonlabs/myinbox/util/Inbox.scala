package com.evalonlabs.myinbox.util

import com.evalonlabs.store.Redis
import java.util.Date

object Inbox extends Object with Redis {

  def add(messageID: String, sender: String, recipient: String, subject: String, gzipMsg: String, date: Date) {
    redis.hset(inboxHSKey(recipient), messageID, gzipMsg)
    redis.sadd(inboxSSKey(recipient), date.getTime, messageID)
    redis.hset(inboxSendersHSKey(recipient), messageID, sender)
    redis.hset(inboxSubjectHSKey(recipient), messageID, subject)
  }

  def index(messageID: String, sender: String, recipient: String, subject: String, msg: String, date: Date) {
    // TODO
  }

  def categorize(uKey: String, messageId: String, category: String) {
    redis.rpush(inboxCategoryLSKey(uKey), category, messageId)
  }

  def tag(uKey: String, messageId: String, tag: String) {
    redis.rpush(inboxTagLSKey(uKey), tag, messageId)
  }
}
