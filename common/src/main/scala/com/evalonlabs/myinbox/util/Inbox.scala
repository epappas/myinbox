package com.evalonlabs.myinbox.util

import com.evalonlabs.myinbox.store.Redis
import java.util.Date

object Inbox extends Object with Redis {

  def add(sender: String, recipient: String, subject: String, gzipMsg: String, date: Date) {
    // TODO
  }

  def index(sender: String, recipient: String, subject: String, msg: String, date: Date) {
    // TODO
  }

  def categorize(uKey: String, messageId: String, category: String) {
    // TODO
  }
}
