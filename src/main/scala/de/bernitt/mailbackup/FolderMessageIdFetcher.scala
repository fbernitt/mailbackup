package de.bernitt.mailbackup

import javax.mail.{FetchProfile, Message, Folder}

class FolderMessageIdFetcher(folder: Folder) {

  def messageIds: Array[String] = {
    withOpenFolder {
      val msgs = folder.getMessages

      prefetchMessageIds(msgs)
      val msgIds = msgs.map(msg => msg.getHeader("Message-Id")(0))

      msgIds
    }
  }

  private def withOpenFolder[T](block: => T): T = {
    if (folder.isOpen)
      block
    else {
      folder.open(Folder.READ_ONLY)
      val result = block
      folder.close(false)
      result
    }
  }

  private def prefetchMessageIds(msgs: Array[Message]) {
    val fetchProfile = new FetchProfile()
    fetchProfile.add(MailBackup.MessageIdHeader)
    folder.fetch(msgs, fetchProfile)
  }
}
