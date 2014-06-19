package com.evalonlabs.myinbox.util

import java.security.MessageDigest
import javax.crypto.spec.SecretKeySpec
import javax.crypto.spec.PBEKeySpec
import javax.crypto.Cipher
import javax.crypto.SecretKeyFactory
import org.subethamail.smtp.util.Base64

object Crypto {

  def md5(str: String) = {
    bytesToHex(MessageDigest.getInstance("MD5").digest(str.getBytes))
  }

  def md2(str: String) {
    bytesToHex(MessageDigest.getInstance("MD2").digest(str.getBytes))
  }

  def sha1(str: String) {
    bytesToHex(MessageDigest.getInstance("SHA-1").digest(str.getBytes))
  }

  def sha256(str: String) {
    bytesToHex(MessageDigest.getInstance("SHA-256").digest(str.getBytes))
  }

  def sha384(str: String) {
    bytesToHex(MessageDigest.getInstance("SHA-384").digest(str.getBytes))
  }

  def sha512(str: String) {
    bytesToHex(MessageDigest.getInstance("SHA-512").digest(str.getBytes))
  }

  def bytesToHex(b: Array[Byte]): String = {
    val hexDigit: Array[Char] = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f')
    val buf: StringBuilder = new StringBuilder
    for (aB <- b) {
      buf.append(hexDigit((aB >> 4) & 0x0f))
      buf.append(hexDigit(aB & 0x0f))
    }
    buf.toString()
  }

  def encrypt(algorithm: String)(bytes: Array[Byte], secret: String, salt: String): Array[Byte] = {
    val factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1")
    val spec = new PBEKeySpec(secret.toCharArray, salt.getBytes, 1024, 128)
    val secretKey = new SecretKeySpec(factory.generateSecret(spec).getEncoded, algorithm)
    val cipher = Cipher.getInstance(algorithm + "/ECB/PKCS5Padding")
    cipher.init(Cipher.ENCRYPT_MODE, secretKey)
    cipher.doFinal(bytes)
  }

  def decrypt(algorithm: String)(bytes: Array[Byte], secret: String, salt: String): Array[Byte] = {
    val factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1")
    val spec = new PBEKeySpec(secret.toCharArray, salt.getBytes, 1024, 128)
    val secretKey = new SecretKeySpec(factory.generateSecret(spec).getEncoded, algorithm)
    val cipher = Cipher.getInstance(algorithm + "/ECB/PKCS5Padding")
    cipher.init(Cipher.DECRYPT_MODE, secretKey)
    cipher.doFinal(bytes)
  }

  def inBase64(bytes: Array[Byte]): String = Base64.encodeToString(bytes, true)

  def fromBase64(str: String): Array[Byte]= Base64.decode(str)

  def inDES(bytes: Array[Byte], secret: String, salt: String): Array[Byte] = encrypt("DES")(bytes: Array[Byte], secret: String, salt: String)

  def inAES(bytes: Array[Byte], secret: String, salt: String): Array[Byte] = encrypt("AES")(bytes: Array[Byte], secret: String, salt: String)

  def inDES64(bytes: Array[Byte], secret: String, salt: String): String = inBase64(inDES(bytes, secret, salt))

  def inAES64(bytes: Array[Byte], secret: String, salt: String): String = inBase64(inAES(bytes, secret, salt))

  def fromDES(bytes: Array[Byte], secret: String, salt: String): Array[Byte] = decrypt("DES")(bytes: Array[Byte], secret: String, salt: String)

  def fromAES(bytes: Array[Byte], secret: String, salt: String): Array[Byte] = decrypt("AES")(bytes: Array[Byte], secret: String, salt: String)

  def fromDES64(str: String, secret: String, salt: String): Array[Byte] = fromDES(fromBase64(str), secret, salt)

  def fromAES64(str: String, secret: String, salt: String): Array[Byte] = fromAES(fromBase64(str), secret, salt)
}
