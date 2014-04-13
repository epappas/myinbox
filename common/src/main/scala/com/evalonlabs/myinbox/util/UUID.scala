package com.evalonlabs.myinbox.util


object UUID {
  def get() = java.util.UUID.randomUUID.toString
}
