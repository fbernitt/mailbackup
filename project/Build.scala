import sbt._
import Keys._

object MailBackupBuild extends Build {

  val copyShell = TaskKey[Unit]("copyShell", "Copy shell files to target dir")

  val copyShellTask = copyShell in(Compile, packageBin) <<= (target, baseDirectory) map {
    (target, baseDir) =>
      println("Copy mail-backup to " + target)
      IO.copyFile(baseDir / "src" / "main" / "shell" / "mail-backup", target / "mail-backup")

      val ownerOnly = true

      (target / "mail-backup").setExecutable(true, !ownerOnly)
  }

  lazy val project = Project("mailbackup", base = file("."), settings = Project.defaultSettings ++ Seq(copyShellTask))
}
