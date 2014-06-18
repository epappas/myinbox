package com.evalonlabs.myinbox.util


object User {

  def getEncryptionKey(uKey: String): String = {
    // TODO get user's key from storage and return, block while waiting
    uKey.concat((1 to 128).foldLeft[String]("")((str, index) => {
      str.concat(String.valueOf((Math.random() * 10 * index).toInt % 10))
    }))
  }

  def getEncryptionSalt(uKey: String): String = {
    // TODO get user's salt from storage and return, block while waiting
    "random-salt"
  }

  def getUKey(mailAddr: String): String = {
    Crypto.md5(mailAddr)
  }

  def addressFromAlias(alias: String): String = {
    // TODO check alias from the store
    alias
  }

  def checkCredentials(uname: String, pass: String, loop: Int): Boolean = {
    // TODO fetch user by this uname
    // TODO IF exists MD5 the pass and check it
    // TODO return true if all steps succeed

    val modPass = (0 to loop).foldLeft[String](pass: String)((pass: String, index: Int) => {
      index % 2 match {
        case 0 => Crypto.md5(pass).toString
        case _ => Crypto.sha1(pass).toString
      }
    })

    // TODO return modPass == realPass
    true
  }
}
