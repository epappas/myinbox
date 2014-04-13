package com.evalonlabs.myinbox.util

import javax.mail.internet.MimeMessage
import javax.mail.{BodyPart, Multipart}
import java.io.ByteArrayOutputStream
import scala.collection.mutable.ArrayBuffer
import scala.Predef.String

object Mail extends Object {

  def filterMultiparts(message: MimeMessage): MimeMessage = {
    val msg = new MimeMessage(message)

    msg.getContentType match {
      case ctype: String if ctype.contains("text") => msg
      case ctype: String if ctype.contains("multipart") => {
        val mpart = msg.getContent.asInstanceOf[Multipart]
        val partList = ArrayBuffer[BodyPart]()

        for(i <- 0 to (mpart.getCount-1)) {
          val targetPart = mpart.getBodyPart(i)

          targetPart.getContentType match {
            case _: String if _.contains("multipart") => {
              val innerMpart = targetPart.getContent.asInstanceOf[Multipart]
              val list = ArrayBuffer[BodyPart]()

              for(j <- 0 to (innerMpart.getCount-1)) {
                val subpart = innerMpart.getBodyPart(j)
                println("subpart " + j + ": " + subpart.getContentType)
                list.append(subpart)
              }

              list.filter(!_.isMimeType("text/*")).foreach(innerMpart.removeBodyPart)

              targetPart.setContent(innerMpart)
            }
            case _ => partList.append(targetPart)
          }
        }

        partList.filter(!_.isMimeType("text/*") && !_.isMimeType("multipart/*"))
          .foreach(mpart.removeBodyPart(_))
        
        msg.setContent(mpart)
        msg.saveChanges()
        msg
      }
      case _ => msg.setContent("unknown", "text/plain")
        msg.saveChanges()
        msg
    }
  }

  def compress(msg: MimeMessage): String = {
    val baos = new ByteArrayOutputStream()
    msg.writeTo(baos)
    Compress.zip(baos.toString("UTF-8"))
  }
}
