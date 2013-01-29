/*
 * Copyright 2013 Folker Bernitt
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package de.bernitt.scalamaildir

import javax.mail.{Address, Folder}
import java.io.{BufferedInputStream, FileInputStream, File, InputStream}
import javax.mail.internet.MimeMessage
import javax.mail.Message.RecipientType
import java.util.Date

object MaildirMessage {
  def create(folder: Folder, file: File, msgNum: Int) = {
    val in = new BufferedInputStream(new FileInputStream(file))

    val msg = new MaildirMessage(folder, MaildirFilename.parseFrom(file.getName), in, msgNum)
    in.close()

    msg
  }
}

//http://stackoverflow.com/questions/3991577/closing-java-inputstreams
//AutoCloseInputStream2
class MaildirMessage private(folder: Folder, val name: MaildirFilename, inputStream: InputStream, msgNum: Int) extends MimeMessage(folder, inputStream, msgNum) {

  setFlags(name.flags, true)

  def isFlagsChanged: Boolean = {
    originalName != nameWithCurrentFlags
  }

  def originalName = name

  def nameWithCurrentFlags: MaildirFilename = {
    val newName = name.cloneWithoutFlags
    newName.addFlags(getFlags)
    newName
  }

  override def setSender(address: Address) {
    throw new UnsupportedOperationException("Not possible to modifiy maildir message")
  }

  override def removeHeader(name: String) {
    throw new UnsupportedOperationException("Not possible to modifiy maildir message")
  }

  override def setRecipients(`type`: RecipientType, addresses: Array[Address]) {
    throw new UnsupportedOperationException("Not possible to modifiy maildir message")
  }

  override def setRecipients(`type`: RecipientType, addresses: String) {
    throw new UnsupportedOperationException("Not possible to modifiy maildir message")
  }

  override def setReplyTo(addresses: Array[Address]) {
    throw new UnsupportedOperationException("Not possible to modifiy maildir message")
  }

  override def addRecipients(`type`: RecipientType, addresses: Array[Address]) {
    throw new UnsupportedOperationException("Not possible to modifiy maildir message")
  }

  override def setSubject(subject: String) {
    throw new UnsupportedOperationException("Not possible to modifiy maildir message")
  }

  override def setSentDate(d: Date) {
    throw new UnsupportedOperationException("Not possible to modifiy maildir message")
  }

  override def addRecipients(`type`: RecipientType, addresses: String) {
    throw new UnsupportedOperationException("Not possible to modifiy maildir message")
  }

  override def setFrom(address: Address) {
    throw new UnsupportedOperationException("Not possible to modifiy maildir message")
  }

  override def setFrom() {
    throw new UnsupportedOperationException("Not possible to modifiy maildir message")
  }

  override def setSubject(subject: String, charset: String) {
    throw new UnsupportedOperationException("Not possible to modifiy maildir message")
  }

  override def setDisposition(disposition: String) {
    throw new UnsupportedOperationException("Not possible to modifiy maildir message")
  }

  override def saveChanges() {
    throw new UnsupportedOperationException("Not possible to modifiy maildir message")
  }
}
