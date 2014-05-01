package com.evalonlabs.myinbox.util

import javax.mail.internet.MimeMessage
import javax.mail.{BodyPart, Multipart}
import java.io.ByteArrayOutputStream
import scala.collection.mutable.ArrayBuffer
import scala.Predef.String
import scala.collection.immutable.Range.Int

object Mail extends Object {

  def filterMultiparts(message: MimeMessage): MimeMessage = {
    val msg = new MimeMessage(message)

    msg.getContentType match {
      case ctype: String if ctype.contains("text") => msg
      case ctype: String if ctype.contains("multipart") => {
        val mpart = msg.getContent.asInstanceOf[Multipart]
        val partList = ArrayBuffer[BodyPart]()

        Range(0, mpart.getCount-1).foreach((i) => {
          val targetPart = mpart.getBodyPart(i)

          targetPart.getContentType match {
            case x: String if x.contains("multipart") => {
              val innerMpart = targetPart.getContent.asInstanceOf[Multipart]
              val list = ArrayBuffer[BodyPart]()

              Range(0, innerMpart.getCount-1).foreach((j) => {
                val subpart = innerMpart.getBodyPart(j)
                //("subpart " + j + ": " + subpart.getContentType)
                list.append(subpart)
              })

              list.filter(!_.isMimeType("text/*")).foreach(innerMpart.removeBodyPart)

              targetPart.setContent(innerMpart)
            }
            case _ => partList.append(targetPart)
          }

        })
        // TODO
//        partList.filter(!_.isMimeType("text/*") && !_.isMimeType("multipart/*"))
//          .foreach(mpart.removeBodyPart(_))
        
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

  def toBytes(msg: MimeMessage): Array[Byte] = {
    val baos = new ByteArrayOutputStream()
    msg.writeTo(baos)
    baos.toByteArray
  }
}
