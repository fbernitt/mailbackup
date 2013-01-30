package de.bernitt.mailbackup

import org.rogach.scallop._
import javax.mail.{Session, URLName}
import java.util.Properties

class Conf(args: Seq[String]) extends ScallopConf(args) {
  val dryRun = toggle("dryRun", short = 'n', default = Some(false))
  val help = toggle("help", default = Some(false))
  val imap = trailArg[String]("imap", required = true)
  val maildir = trailArg[String]("maildir", required = true)
}


object Main {
  def main(args: Array[String]) {
    val conf = new Conf(args)
    if (conf.help()) {
      conf.printHelp()
    } else {
      doBackup(conf.imap(), conf.maildir())
    }
  }

  def doBackup(imapUrl: String, maildirPath: String) {
    val imapUrlName = new URLName(imapUrl)

    println(imapUrlName.getUsername)
    println(imapUrlName.getHost)

    val imapStore = createImapStore(imapUrlName)
    imapStore.close()
  }

  def createImapStore(imapUrl: URLName) = {
    val props = new Properties()
    val session = Session.getDefaultInstance(props)
    val store = session.getStore("imap")
    store.connect(imapUrl.getHost, imapUrl.getUsername, "")
    store
  }
}
