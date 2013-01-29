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

import org.scalatest._

import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito._
import org.mockito.Matchers.{eq => the}
import java.io.File
import javax.mail.{Session, FolderNotFoundException, Folder}
import javax.mail.internet.MimeMessage
import java.util.Properties
import javax.mail.Flags.Flag

trait TemporaryDirectory extends BeforeAndAfterEach {
  this: Suite =>

  protected val tempDir = createTemporaryDirectory()

  override def beforeEach() {
    tempDir.mkdir()
    if (!tempDir.exists() || !tempDir.isDirectory)
      throw new RuntimeException("Failed to create temporary dir " + tempDir.getAbsolutePath)
    super.beforeEach()
  }

  override def afterEach() {
    deleteDirectory(tempDir)
    if (tempDir.exists()) throw new RuntimeException("Failed to delete temp dir " + tempDir.getAbsolutePath)
    super.afterEach()
  }

  private def createTemporaryDirectory(): File = {
    val file = File.createTempFile("test", "dir")
    file.delete()
    file
  }

  private def deleteDirectory(dir: File): Boolean = {
    if (dir.exists()) {
      dir.listFiles().foreach {
        path =>
          if (path.isDirectory) {
            deleteDirectory(path)
          } else {
            path.delete()
          }
      }
    }
    dir.delete()
  }
}

class MaildirFolderTest extends FlatSpec with ShouldMatchers with BeforeAndAfterEach with TemporaryDirectory with MockitoSugar {

  val store = mock[MaildirJavaMailStore]

  override def beforeEach() {
    reset(store)
    when(store.maildirRoot).thenReturn(tempDir)
  }

  "A MaildirFolder" should "accept store and filename in ctx" in {
    new MaildirFolder("INBOX", store).getName should equal("INBOX")
    new MaildirFolder("INBOX", store).getFullName should equal("INBOX")
  }

  "A MaildirFolder" should "support a default folder" in {
    val store = mock[MaildirJavaMailStore]

    new MaildirFolder(".", store, true).getName should equal(".")
  }

  "A MaildirFolder" should "should allow sub folders" in {
    new MaildirFolder("foo/bar", store).getName should equal("bar")
    new MaildirFolder("foo/bar", store).getFullName should equal("foo/bar")
  }

  "A MaildirFolder" should "provide access to its parent folder" in {
    new MaildirFolder("foo/bar", store).getParent.getFullName should equal("foo")
    new MaildirFolder("foo", store).getParent should equal(null)
  }

  "A MaildirFolder" should "allow to check whether it exist" in {
    val folder = new MaildirFolder("foobar", store)

    folder.exists() should equal(false)

    val createResult = folder.create(Folder.HOLDS_FOLDERS | Folder.HOLDS_MESSAGES)

    createResult should equal(true)
    folder.exists() should equal(true)
  }

  "A MaildirFolder" should "be created with new/cur/tmp" in {
    val folder = new MaildirFolder("foobar", store)
    val foobarFile = new File(tempDir, ".foobar")

    folder.create(FolderFlags.DEFAULT.flags)

    new File(foobarFile, "new").exists() should be(true)
    new File(foobarFile, "cur").exists() should be(true)
    new File(foobarFile, "tmp").exists() should be(true)
  }

  "A MaildirFolder" should "not be createable with subfolders if its INBOX" in {
    val inbox = new MaildirFolder("INBOX", store)

    inbox should not be (null)
    evaluating(inbox.create(Folder.HOLDS_FOLDERS)) should produce[IllegalArgumentException]
  }

  "A MaildirFolder" should "not be createable with messages if its default" in {
    val defaultFolder = MaildirFolder.defaultFolder(store)

    evaluating(defaultFolder.create(Folder.HOLDS_MESSAGES)) should produce[IllegalArgumentException]
  }

  "A MaildirFolder" should "allow to delete an existing folder" in {
    val folder = new MaildirFolder("foobar", store)
    folder.create(Folder.HOLDS_FOLDERS | Folder.HOLDS_MESSAGES)

    folder.delete(false)

    folder.exists() should be(false)
  }

  "A MaildirFolder" should "ask store on call of getFolder" in {
    val store = mock[MaildirJavaMailStore]
    val fooFolder = new MaildirFolder("foo", store)
    val barFolder = new MaildirFolder("foo/bar", store)

    when(store.getFolder("foo")).thenReturn(fooFolder)
    when(store.getFolder("foo/bar")).thenReturn(barFolder)

    fooFolder.getFolder("bar") should equal(barFolder)
    fooFolder.getFolder("/foo/bar") should equal(barFolder)
    fooFolder.getFolder("/foo") should equal(fooFolder)
  }

  "A MaildirFolder" should "should provide supported flags" in {
    val store = mock[MaildirJavaMailStore]
    val folder = new MaildirFolder("foo", store)

    folder.getPermanentFlags should equal(MaildirFolder.supportedFlags)
  }

  "A MaildirFolder" should "should list its subfolders" in {
    val inboxFolder = createInboxFolder()
    val fooFolder = createFolder("foo", store)
    val barFolder = createFolder("foo/bar", store)
    val defaultFolder = MaildirFolder.defaultFolder(store)

    inboxFolder.list() should equal(Array())
    defaultFolder.list().size should equal(2)
    fooFolder.list().size should equal(1)
    barFolder.list().size should equal(0)

  }

  "A MaildirFolder" should "should accept wildcard to list its subfolders" in {
    val fooFolder = createFolder("foo", store)
    createFolder("foo/bar", store)
    val defaultFolder = MaildirFolder.defaultFolder(store)

    defaultFolder.list("%/bar").size should equal(1)
    defaultFolder.list("f%").size should equal(1)
    defaultFolder.list("f*").size should equal(2)
    fooFolder.list("b%").size should equal(1)
    fooFolder.list("none").size should equal(0)
  }

  "A MaildirFolder" should "should throw IllegalStateException on open if already open" in {
    val folder = createFolder("foo", store)
    folder.open(Folder.READ_ONLY)

    evaluating(folder.open(Folder.READ_ONLY)) should produce[IllegalStateException]
  }

  "A MaildirFolder" should "should throw IllegalStateException on close if not open" in {
    val folder = createFolder("foo", store)

    evaluating(folder.close(false)) should produce[IllegalStateException]
  }

  "A MaildirFolder" should "should throw IllegalStateException on operations on closed folder" in {
    val folder = createFolder("foo", store)

    evaluating(folder.getMessage(1)) should produce[IllegalStateException]
  }

  "A MaildirFolder" should "should throw FolderNotFoundException if folder does not exist" in {
    val folder = new MaildirFolder("foobar", store)

    evaluating(folder.hasNewMessages) should produce[FolderNotFoundException]
    evaluating(folder.open(0)) should produce[FolderNotFoundException]
    evaluating(folder.getMessage(1)) should produce[FolderNotFoundException]
  }

  "A MaildirFolder" should "be renamable" in {
    val folder = createFolder("folder", store)
    val newFolderName = new MaildirFolder("newName", store)


    folder.renameTo(newFolderName) should be(true)
    newFolderName.exists() should equal(true)
    folder.exists() should be(false)
  }

  "A MaildirFolder" should "not be renameable if it not exists or is closed" in {
    val notExistingFolder = new MaildirFolder("foobar", store)
    val openFolder = createFolder("openFolder", store)
    val newFolderName = new MaildirFolder("newFolderName", store)

    openFolder.open(Folder.READ_WRITE)

    evaluating(notExistingFolder.renameTo(newFolderName)) should produce[FolderNotFoundException]
    evaluating(openFolder.renameTo(newFolderName)) should produce[IllegalStateException]
  }

  "A MaildirFolder" should "not be renameable if target folder already exists" in {
    val folder = createFolder("openFolder", store)
    val newFolderName = createFolder("newFolderName", store)

    evaluating(folder.renameTo(newFolderName)) should produce[IllegalArgumentException]
  }

  "A MaildirFolder" should "should allow to append messages" in {
    val folder = createFolder("foobar", store)
    folder.open(Folder.READ_WRITE)

    val msg = new MimeMessage(defaultSession)
    msg.setText("some message")

    folder.appendMessages(Array(msg))

    new File(tempDir, ".foobar/cur").list().size should equal(1)
    folder.getMessageCount should equal(1)
  }

  "A MaildirFolder" should "expunge deleted messages" in {
    val folder = createFolder("foobar", store)
    folder.open(Folder.READ_WRITE)
    folder.appendMessages(Array(createMessage("Subject", "Body")))

    val msg = folder.getMessage(1)
    msg.setFlag(Flag.DELETED, true)

    folder.getDeletedMessageCount should equal(1)

    val expungedMsgs = folder.expunge()

    folder.getMessageCount should equal(0)
    expungedMsgs.size should equal(1)
    folder.getDeletedMessageCount should equal(0)
  }

  "A MaildirFolder" should "not be expungeable if it does not exist or is not opened RW" in {
    val notExist = new MaildirFolder("does-not-exist", store)
    val notOpened = createFolder("not-opened", store)
    val openedOnly = createFolder("readonly", store)

    evaluating(notExist.expunge()) should produce[FolderNotFoundException]
    evaluating(notOpened.expunge()) should produce[IllegalStateException]
    evaluating(openedOnly.expunge()) should produce[IllegalStateException]
  }

  "A MaildirFolder" should "be expunged during close in " in {
    val folder = createFolder("foobar", store)
    folder.open(Folder.READ_WRITE)
    folder.appendMessages(Array(createMessage("Subject", "Body")))

    val msg = folder.getMessage(1)
    msg.setFlag(Flag.DELETED, true)

    folder.close(true)
    folder.open(Folder.READ_ONLY)

    folder.getDeletedMessageCount should equal(0)
    folder.getMessageCount should equal(0)
  }

  "A MaildirFolder" should "update message flags on close" in {
    val folder = createFolder("foobar", store)
    folder.open(Folder.READ_WRITE)
    folder.appendMessages(Array(createMessage("Subject", "Body")))

    val msg = folder.getMessage(1).asInstanceOf[MaildirMessage]
    msg.setFlag(Flag.ANSWERED, true)
    msg.setFlag(Flag.SEEN, true)

    folder.close(false)

    new File(tempDir, ".foobar/cur/" + msg.name.name).exists() should be(false)
    new File(tempDir, ".foobar/cur/" + msg.name.uniq + ":2,RS").exists() should be(true)
  }

  private def createFolder(name: String, store: MaildirJavaMailStore): MaildirFolder = {
    val folder = new MaildirFolder(name, store)
    folder.create(FolderFlags.DEFAULT.flags)
    folder
  }

  private def createInboxFolder(): MaildirFolder = {
    val inbox = new MaildirFolder("INBOX", store)
    inbox.create(FolderFlags.INBOX_FLAGS.flags)
    inbox
  }

  private def defaultSession = Session.getDefaultInstance(new Properties())

  private def createMessage(subject: String, body: String) = {
    val msg = new MimeMessage(defaultSession)
    msg.setText(body)
    msg.setSubject(subject)
    msg
  }
}
