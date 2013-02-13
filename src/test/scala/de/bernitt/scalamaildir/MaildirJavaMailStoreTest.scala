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

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import javax.mail.{URLName, Session}
import util.TemporaryDirectory
import javax.mail
import mail.Provider.Type

class MaildirJavaMailStoreTest extends FlatSpec with ShouldMatchers with TemporaryDirectory {
  val NoAutoCreate = false
  val AutoCreate = true

  "A MaildirJavaMailStore" should "be registered as provider" in {
    val provider = createSession().getProvider("maildir")

    provider should not be null
    provider.getType should equal(Type.STORE)
    provider.getClassName should equal(classOf[MaildirJavaMailStore].getName)
  }

  "A MaildirJavaMailStore" should "use the provided url as root maildir" in {
    val session = createSession(NoAutoCreate)
    val store = session.getStore(maildirUrl)

    store.asInstanceOf[MaildirJavaMailStore].maildirRoot should equal(tempDir)
  }


  "A MaildirJavaMailStore companion" should "create a session with auto create flag" in {
    val sessionWithAutoCreate = MaildirJavaMailStore.createSession(AutoCreate)
    val sessionWithoutAutoCreate = MaildirJavaMailStore.createSession(NoAutoCreate)

    sessionWithAutoCreate.getProperty(MaildirJavaMailStore.AutoCreateProperty) should equal("true")
    sessionWithoutAutoCreate.getProperty(MaildirJavaMailStore.AutoCreateProperty) should equal("false")
  }

  "A MaildirJavaMailStore" should "not auto create inbox if auto create property is set to false" in {
    forceMaildirDoesNotExist()

    val session = createSession(NoAutoCreate)
    val store = session.getStore(maildirUrl)

    store should not be null
    store.getFolder("INBOX").exists() should be(NoAutoCreate)
  }


  def forceMaildirDoesNotExist() {
    tempDir.delete()
  }

  "A MaildirJavaMailStore" should "auto create folders" in {
    forceMaildirDoesNotExist()

    val session = createSession(AutoCreate)
    val store = session.getStore(maildirUrl)

    store should not be null
    store.getFolder("INBOX").exists() should be(right = true)
  }

  "A MaildirJavaMailStore" should "should return a default folder" in {
    val store = createSession().getStore(maildirUrl)

    val folder = store.getDefaultFolder

    folder should not be null
    folder.getName should equal(".")
  }

  "A MaildirJavaMailStore" should "should extract folder name from an URLName" in {
    val store = createSession().getStore(maildirUrl)

    val folder = store.getFolder(new URLName(maildirUrl.toString + "/testFolder"))

    folder should not be null
    folder.getName should equal("testFolder")
  }

  private def maildirUrl: URLName = {
    new URLName("maildir:///" + tempDir.getAbsolutePath)
  }

  private def createSession(autoCreate: Boolean = NoAutoCreate): Session = {
    val props = new java.util.Properties()
    props.setProperty(MaildirJavaMailStore.AutoCreateProperty, autoCreate.toString)
    props.setProperty("mail.debug", false.toString)

    val session = Session.getInstance(props)

    session
  }
}
