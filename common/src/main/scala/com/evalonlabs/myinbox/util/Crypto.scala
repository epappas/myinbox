package com.evalonlabs.myinbox.util

import java.security.MessageDigest

object Crypto {
  def md5(str: String) {
    MessageDigest.getInstance("MD5").digest(str.getBytes)
  }
  def md2(str: String) {
    MessageDigest.getInstance("MD2").digest(str.getBytes)
  }
  def sha1(str: String) {
    MessageDigest.getInstance("SHA-1").digest(str.getBytes)
  }
  def sha256(str: String) {
    MessageDigest.getInstance("SHA-256").digest(str.getBytes)
  }
  def sha384(str: String) {
    MessageDigest.getInstance("SHA-384").digest(str.getBytes)
  }
  def sha512(str: String) {
    MessageDigest.getInstance("SHA-384").digest(str.getBytes)
  }
}
