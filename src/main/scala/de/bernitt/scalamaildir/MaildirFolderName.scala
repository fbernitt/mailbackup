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

import java.io.File


class MaildirFolderName(folderName: String) {

  def name = folderName.split(File.separatorChar).last

  def fullName = folderName

  def maildirName: String = if (name == "INBOX") "." else '.' + buildMaildirName

  def base64(original: String) = MaildirBase64Encoder.encode(original)

  def hasParent: Boolean = folderName.contains(File.separatorChar)

  def parent: MaildirFolderName = {
    if (hasParent)
      new MaildirFolderName(folderName.substring(0, folderName.lastIndexOf(File.separatorChar)))
    else
      throw new UnsupportedOperationException(name + " does not have a parent folder!")
  }


  override def equals(other: Any): Boolean = other match {
    case other: MaildirFolderName => other.fullName == fullName
    case _ => false
  }


  override def hashCode(): Int = folderName.hashCode

  def pathRelativeTo(file: File): File = {
    if (isInbox)
      file
    else
      new File(file, maildirName)
  }

  private def buildMaildirName: String = {
    folderName.split('/').map(base64(_)).mkString(".")
  }

  def isInbox = name == "INBOX"
}
