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

import io.LocalPlatformNewlineFilterOutputStream
import javax.mail.{FolderNotFoundException, Flags, Message, Folder}
import java.io.{BufferedOutputStream, FileOutputStream, File}
import collection.mutable.ArrayBuffer
import javax.mail.Flags.Flag
import java.io.OutputStream

object MaildirFolder {
  def defaultFolder(store: MaildirJavaMailStore) = {
    new MaildirFolder(".", store, true)
  }

  val supportedFlags = createSupportedFlags

  private def createSupportedFlags: Flags = {
    val flags = new Flags()
    flags.add(Flags.Flag.ANSWERED)
    flags.add(Flags.Flag.DELETED)
    flags.add(Flags.Flag.DRAFT)
    flags.add(Flags.Flag.FLAGGED)
    flags.add(Flags.Flag.RECENT)
    flags.add(Flags.Flag.SEEN)

    flags
  }

}

object FolderFlags {
  val INBOX_FLAGS = new FolderFlags(Folder.HOLDS_MESSAGES)
  val DEFAULT_FOLDER_FLAGS = new FolderFlags(Folder.HOLDS_MESSAGES)

  val DEFAULT = new FolderFlags(Folder.HOLDS_MESSAGES | Folder.HOLDS_FOLDERS)
}

class FolderFlags(val flags: Int) {

  def asReadOnly = new FolderFlags(flags | Folder.READ_ONLY)

  def isReadOnly = (flags & Folder.READ_ONLY) > 0

  def isHoldsMessages = (flags & Folder.HOLDS_MESSAGES) > 0

  def isHoldsFolders = (flags & Folder.HOLDS_FOLDERS) > 0
}

class MaildirFolder(name: String, store: MaildirJavaMailStore, defaultFolder: Boolean) extends Folder(store) {

  def this(name: String, store: MaildirJavaMailStore) = this(name, store, false)

  val maildirName = new MaildirFolderName(name)

  var flags = if (maildirName.isInbox) FolderFlags.INBOX_FLAGS else if (defaultFolder) FolderFlags.DEFAULT_FOLDER_FLAGS else FolderFlags.DEFAULT

  val rootDir = store.maildirRoot

  var opened = false

  var messages = new ArrayBuffer[MaildirMessage]

  override def getName: String = maildirName.name

  override def getFullName: String = maildirName.fullName

  override def getParent: Folder = if (maildirName.hasParent) new MaildirFolder(maildirName.parent.fullName, store) else null.asInstanceOf[MaildirFolder]

  override def exists(): Boolean = folder.exists()

  override def list(pattern: String): Array[Folder] = {
    if (maildirName.isInbox)
      return Array()

    val prefix = if (defaultFolder) "" else maildirName.fullName + "/"
    val matcher = new FolderPatternMatcher(prefix + pattern)
    val files = rootDir.list().filter(name => name.startsWith(".")).filter(name => matcher.matchesEncoded(name))
    val encodedNames = files.map(_.substring(1)).map(file => file.split(".").map(decodeBase64(_)).mkString("/"))

    val result = new ArrayBuffer[Folder]
    if (matcher.matchesEncoded("INBOX")) {
      result.append(new MaildirFolder("INBOX", store))
    }
    result.appendAll(encodedNames.map(new MaildirFolder(_, store)))

    result.toArray
  }

  private def decodeBase64(str: String): String = MaildirBase64Decoder.decode(str)

  override def getSeparator: Char = '/'

  override def getType: Int = flags.flags

  override def create(typeFlags: Int): Boolean = {
    if (maildirName.isInbox && (typeFlags & Folder.HOLDS_FOLDERS) > 0)
      throw new IllegalArgumentException("INBOX folder may not contain other folders")

    if (defaultFolder && (typeFlags & Folder.HOLDS_MESSAGES) > 0)
      throw new IllegalArgumentException("Default folder may not contain messages")

    folder.mkdirs()
    newDir.mkdir()
    tmpDir.mkdir()
    curDir.mkdir()
  }

  private def newDir = new File(folder, "new")

  private def tmpDir = new File(folder, "tmp")

  private def curDir = new File(folder, "cur")

  private def folder = maildirName.pathRelativeTo(rootDir)

  override def hasNewMessages: Boolean = {
    if (!exists()) {
      throw new FolderNotFoundException(this, "a folder named " + getFullName + " does not exist!")
    }
    countMessagesIn(newDir) > 0
  }

  override def getFolder(name: String): Folder = if (name.startsWith(getSeparator.toString)) store.getFolder(name.substring(1)) else store.getFolder(getFullName + getSeparator + name)

  override def delete(recurse: Boolean): Boolean = {
    if (recurse) throw new RuntimeException("not yet implemented")

    if (maildirName.isInbox || defaultFolder)
      throw new UnsupportedOperationException("Not possible to delete either INBOX or default folder")

    if (exists())
      deleteDirectory(folder)
    else
      true
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

  override def renameTo(newName: Folder): Boolean = {
    if (!exists()) {
      throw new FolderNotFoundException(this, "a folder named " + getFullName + " does not exist!")
    }
    if (isOpen) {
      throw new IllegalStateException("Folder is currently open")
    }
    if (newName.exists()) {
      throw new IllegalArgumentException("Target folder already exists!")
    }

    folder.renameTo(newName.asInstanceOf[MaildirFolder].folder)
  }

  def autoRepair() {
    List(newDir, curDir, tmpDir).foreach(_.mkdir())
  }

  override def open(mode: Int) {
    if (!exists()) {
      throw new FolderNotFoundException(this, "a folder named " + getFullName + " does not exist!")
    }
    if (isOpen) {
      throw new IllegalStateException("Folder already opened")
    }
    this.mode = mode

    if (isAutoRepairNecessary && !isReadOnly && isAutoRepair) {
      autoRepair()
    }


    opened = true


    reloadMessages()
    if (!isReadOnly) {
      moveFromNewToCur()
    }
  }

  private def isAutoRepairNecessary: Boolean = folder.exists() && (!curDir.exists() || !newDir.exists() || !tmpDir.exists())

  private def isAutoRepair = store.sessionProperties.getProperty("mail.store.maildir.autorepair", "true").toBoolean

  private def reloadMessages() {
    val msgs = readMessages
    messages = new ArrayBuffer[MaildirMessage]()

    msgs.foreach(messages.append(_))
  }

  private def readMessages = {
    val newMsgs = readNewMsgs(1)
    val curMsgs = readCurMsgs(newMsgs.size + 1)

    newMsgs ++ curMsgs
  }

  private def readNewMsgs(startMsgNum: Int) = {
    val msgs = readMessagesIn(newDir, startMsgNum)
    msgs.foreach(_.setFlag(Flag.RECENT, true))
    msgs
  }

  private def readCurMsgs(startMsgNum: Int) = readMessagesIn(curDir, startMsgNum)

  private def isReadOnly: Boolean = (mode & Folder.READ_ONLY) > 0

  private def moveFromNewToCur() {
    if (!newDir.exists() || !curDir.exists())
      return

    val newMails = newDir.listFiles().toList

    newMails.foreach {
      mail: File =>
        println("Moving file " + mail)
        val newFilename = MaildirFilename.parseFrom(mail.getName)
        val curFilename = newFilename.cloneWithoutFlags
        curFilename.setFlag(Flag.RECENT)
        moveTo(new File(newDir, newFilename.name), new File(curDir, curFilename.name))
    }
  }

  private def readMessagesIn(dir: File, startMsgNum: Int): Array[MaildirMessage] = {
    val files = if (dir.exists()) dir.listFiles().toList else List[File]()

    var msgNum = startMsgNum
    files.map {
      msgNum = msgNum + 1
      MaildirMessage.create(this, _, msgNum)
    }.toArray
  }

  override def close(expungeOnClose: Boolean) {
    if (!isOpen) {
      throw new IllegalStateException("Folder is not open")
    }
    if (expungeOnClose) {
      expunge()
    }

    if (!isReadOnly) {
      renameMessagesToTheirCurrentFlagState
    }

    messages = new ArrayBuffer[MaildirMessage]()

    opened = false
  }


  private def renameMessagesToTheirCurrentFlagState {
    for (msg <- messages) {
      if (msg.isFlagsChanged) {
        moveTo(new File(curDir, msg.originalName.name), new File(curDir, msg.nameWithCurrentFlags.name))
      }
    }
  }

  override def isOpen: Boolean = opened

  override def getPermanentFlags: Flags = MaildirFolder.supportedFlags

  override def getMessageCount: Int = {
    if (isOpen) {
      messages.size
    } else {
      countMessagesIn(newDir) + countMessagesIn(curDir)
    }
  }


  private def countMessagesIn(dir: File): Int = if (dir.exists()) dir.listFiles().size else 0

  override def getMessage(msgNum: Int): Message = {
    if (!exists()) {
      throw new FolderNotFoundException(this, "a folder named " + getFullName + " does not exist!")
    }
    if (!isOpen) {
      throw new IllegalStateException("Folder is in closed state!")
    }

    if (msgNum <= 0 || msgNum > messages.size) {
      throw new IndexOutOfBoundsException("Only " + messages.size + " messages available! " + msgNum + " is out of bounds!")
    }

    messages(msgNum - 1)
  }

  override def appendMessages(msgs: Array[Message]) {
    for (msg <- msgs) {
      val tmpFilename = MaildirFilename.create()
      val tmpFile = new File(tmpDir, tmpFilename.name)
      writeToFile(msg, tmpFile)

      if (deliverToNew(msg)) {
        val newFile = new File(newDir, tmpFilename.name)
        moveTo(tmpFile, newFile)
        appendToMessagesIfOpen(newFile)
      } else {
        val curFilename = tmpFilename.cloneWithoutFlags
        curFilename.addFlags(msg.getFlags)

        val curFile = new File(curDir, curFilename.name)
        moveTo(tmpFile, curFile)
        appendToMessagesIfOpen(curFile)
      }
    }
  }

  private def appendToMessagesIfOpen(file: File) {
    if (isOpen) {
      val msg = MaildirMessage.create(this, file, messages.size + 1)
      msg.setFlag(Flag.RECENT, true)
      messages.append(msg)
    }
  }

  override def expunge(): Array[Message] = {
    if (!exists()) {
      throw new FolderNotFoundException(this, "a folder named " + getFullName + " does not exist!")
    }
    if (!isOpen) {
      throw new IllegalStateException("Folder is in closed state!")
    }
    if (isOpen && isReadOnly) {
      throw new IllegalStateException("Folder has been opend readonly!")
    }

    val expungedMsgs = new ArrayBuffer[Message]

    for (msg <- messages) {
      if (msg.isSet(Flag.DELETED)) {
        val file = new File(curDir, msg.name.name)
        if (file.delete()) {
          expungedMsgs.append(msg)
        }
      }
    }

    reloadMessages()

    expungedMsgs.toArray
  }

  private def isInboxFolder: Boolean = name.equals(".")


  private def deliverToNew(msg: Message): Boolean = (!opened) || msg.isSet(Flag.RECENT)

  private def moveTo(src: File, dst: File) {
    src.renameTo(dst)
  }

  private def writeToFile(msg: Message, file: File) {
    val out = createOutputStream(file)
    msg.writeTo(out)
    out.close()
  }

  private def createOutputStream(file: File): OutputStream = new LocalPlatformNewlineFilterOutputStream(new BufferedOutputStream(new FileOutputStream(file), 4096))

}
