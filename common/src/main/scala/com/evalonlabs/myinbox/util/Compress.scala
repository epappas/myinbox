package com.evalonlabs.myinbox.util

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import scala.io.Source


object Compress {

  def zip(in: String): String = {
    val baos = new ByteArrayOutputStream()
    val gzip = new GZIPOutputStream(baos)
    gzip.write(in.getBytes("ISO-8859-1"))
    gzip.close()
    baos.toString("ISO-8859-1")
  }

  def unzip(in: String): String = {
    Source.fromInputStream(
      new GZIPInputStream(new ByteArrayInputStream(in.getBytes("ISO-8859-1")))
    ).mkString
  }
}
