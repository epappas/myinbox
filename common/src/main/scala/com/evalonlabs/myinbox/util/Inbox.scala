package com.evalonlabs.myinbox.util

import com.evalonlabs.store.Redis
import java.util.Date

object Inbox extends Object with Redis {

  def add(messageID: String, sender: String, recipient: String, subject: String, gzipMsg: String, date: Date) {
    // TODO
  }

  def index(messageID: String, sender: String, recipient: String, subject: String, msg: String, date: Date) {
    // TODO
  }

  def categorize(uKey: String, messageId: String, category: String) {
    // TODO
  }

  def tag(uKey: String, messageId: String, tag: String) {
    // TODO
  }
}
