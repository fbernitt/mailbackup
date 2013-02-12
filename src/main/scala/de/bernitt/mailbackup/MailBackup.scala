package de.bernitt.mailbackup

import javax.mail.{Message, FetchProfile, Folder, Store}
import collection.mutable.ArrayBuffer


object MailBackup {
  val MessageIdHeader = "Message-Id"
  val AllFoldersPattern = "*"
}

class MailBackup(srcStore: Store, dstStore: Store, dryRun: Boolean = false) {

  trait MessageWrapper {
    def firstHeader(name: String): String
  }

  implicit def toFolderWrapper(msg: Message) = new MessageWrapper {
    def firstHeader(name: String): String = msg.getHeader(name)(0)
  }

  val verbose = true

  def backup() {
    folders.foreach(backupFolder(_))
  }

  private def backupFolder(srcFolder: Folder) {
    val dstFolder = createDstFolder(srcFolder.getFullName)

    try {
      srcFolder.open(Folder.READ_ONLY)
      dstFolder.open(Folder.READ_WRITE)

      val missingMsgIds = missingMessageIds(dstFolder, srcFolder)
      if (messagesForBackup(missingMsgIds)) {
        val missingMsgs = extractMissingMessages(srcFolder.getMessages, missingMsgIds)
        backupMessages(missingMsgs, dstFolder)
      }
    } finally {
      srcFolder.close(false)
      dstFolder.close(false)
    }
  }

  private def backupMessages(missingMsgs: Array[Message], dstFolder: Folder) {
    val count = missingMsgs.size
    val chunks = missingMsgs.grouped(100).toList

    vprint(dstFolder.getName + ": backup " + count + " messages")

    for (chunk <- chunks) {
      dstFolder.appendMessages(chunk)

    }
    vprintln("done")
  }

  private def messagesForBackup(missingMsgIds: Set[String]): Boolean = {
    !missingMsgIds.isEmpty
  }

  private def extractMissingMessages(msgs: Array[Message], missingIds: Set[String]): Array[Message] = msgs.filter(msg => missingIds.contains(msg.firstHeader("Message-Id")))

  private def missingMessageIds(dstFolder: Folder, srcFolder: Folder): Set[String] = {
    val localMsgIds = messageIdsOf(dstFolder)
    val remoteMsgIds = messageIdsOf(srcFolder)

    remoteMsgIds -- localMsgIds
  }

  private def createDstFolder(name: String) = {
    val folder = dstStore.getFolder(name)
    if (!folder.exists()) {
      folder.create(Folder.HOLDS_FOLDERS | Folder.HOLDS_MESSAGES)
    }
    folder
  }

  private def messageIdsOf(folder: Folder): Set[String] = {
    val msgs = folder.getMessages

    prefetchMessageIds(folder, msgs)

    val msgIds = new ArrayBuffer[String]()

    for (msg <- msgs) {
      val id: String = msg.firstHeader(MailBackup.MessageIdHeader)

      msgIds += id
    }

    msgIds.toArray.toSet
  }

  private def prefetchMessageIds(folder: Folder, msgs: Array[Message]) {
    val fetchProfile = new FetchProfile()
    fetchProfile.add(MailBackup.MessageIdHeader)
    folder.fetch(msgs, fetchProfile)
  }

  private def folders: Array[Folder] = srcStore.getDefaultFolder.list(MailBackup.AllFoldersPattern)

  private def vprint(x: Any) {
    if (verbose) {
      print(x)
    }
  }

  private def vprintln(x: Any) {
    if (verbose) {
      println(x)
    }
  }
}