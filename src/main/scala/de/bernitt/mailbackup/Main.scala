package de.bernitt.mailbackup

import java.io.File
import org.rogach.scallop._
import javax.mail.{Session, URLName}
import java.util.Properties

class Conf(args: Seq[String]) extends ScallopConf(args) {
  val dryRun = toggle("dryRun", short = 'n', default = Some(false))
  val help = toggle("help", default = Some(false), descrYes = "Show this help")
  val imap = trailArg[String]("imap", required = true)
  val maildir = trailArg[String]("maildir", required = true)
}


object Main {
  def main(args: Array[String]) {
    val conf = new Conf(args)
    if (conf.help()) {
      conf.printHelp()
    } else {
      doBackup(conf.imap(), conf.maildir(), conf.dryRun())
    }
  }

  def doBackup(imapUrl: String, maildirPath: String, dryRun: Boolean) {
    val imapUrlName = new URLName(imapUrl)
    val imapStore = createImapStore(imapUrlName)
    val localStore = createMaildirStore(new File(maildirPath))
    try {
      new MailBackup(imapStore, localStore, dryRun).backup()
    } finally {
      imapStore.close()
      localStore.close()
    }
  }

  private def createImapStore(imapUrl: URLName) = {
    val store = defaultSession.getStore(imapUrl.getProtocol)
    val password = System.console().readPassword("IMAP Password:").mkString
    store.connect(imapUrl.getHost, imapUrl.getUsername, password)
    store
  }

  private def createMaildirStore(folder: File) = {
    defaultSession.getStore(new URLName("maildir:///" + folder.getAbsolutePath))
  }

  private def defaultSession: Session = {
    val props = new Properties()

    Session.getDefaultInstance(props)
  }
}
