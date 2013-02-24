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

import javax.mail.{Folder, URLName, Session, Store}
import java.util.Properties
import java.io.File

object MaildirJavaMailStore {
  val MaildirUrlPrefix = "maildir:///"
  val AutoCreateProperty = "mail.store.maildir.autocreate"
  val NoAutoCreate = false

  def createSession(autoCreate: Boolean): Session = {
    val props = new Properties()
    props.put(AutoCreateProperty, autoCreate.toString)
    Session.getInstance(props)
  }

  def fileToUrlName(file: File) = new URLName(MaildirUrlPrefix + file.getAbsolutePath)
}

class MaildirJavaMailStore(session: Session, urlName: URLName) extends Store(session, urlName) {

  def this(file: File) = this(MaildirJavaMailStore.createSession(MaildirJavaMailStore.NoAutoCreate), MaildirJavaMailStore.fileToUrlName(file))

  def this(file: File, autoCreate: Boolean) = this(MaildirJavaMailStore.createSession(autoCreate), MaildirJavaMailStore.fileToUrlName(file))

  if (isAutoCreate) {
    getFolder("INBOX").create(Folder.HOLDS_MESSAGES)
  }

  override def getDefaultFolder: Folder = MaildirFolder.defaultFolder(this)

  override def getFolder(name: String): Folder = new MaildirFolder(name, this)

  override def getFolder(url: URLName): Folder = new MaildirFolder(url.getFile, this)

  def maildirRoot = new File(urlName.getFile)

  def sessionProperties = session.getProperties

  private def isAutoCreate = session.getProperties.getProperty(MaildirJavaMailStore.AutoCreateProperty, "false").toBoolean
}
