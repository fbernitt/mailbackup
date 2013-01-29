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
import collection.mutable.ArrayBuffer
import java.net.InetAddress
import javax.mail.Flags.Flag

class MaildirFilenameTest extends FlatSpec with ShouldMatchers {

  "A MaildirFilename" should "be created with a uniq name" in {
    val buffer = new ArrayBuffer[String]

    for (i <- 1 to 10) {
      buffer.append(MaildirFilename.create().name)
    }

    buffer.toSet.size should equal(10)
  }

  "A MaildirFilename" should "match pattern without flags" in {
    val fileName = MaildirFilename.create()
    ("[0-9]+[.][0-9]+_[0-9]+[.]" + hostname).r.pattern.matcher(fileName.name).matches should equal(true)
  }

  "A MaildirFilename" should "append flags" in {
    val fileName = MaildirFilename.create()

    val name = fileName.name

    fileName.setFlag(Flag.ANSWERED)
    fileName.setFlag(Flag.DELETED)
    fileName.setFlag(Flag.DRAFT)
    fileName.setFlag(Flag.FLAGGED)
    fileName.setFlag(Flag.SEEN)

    name + ":2,DFRST" should equal(fileName.name)
  }

  "A MaildirFilename" should "be able to parse existing filename" in {
    val name = "1358675765.03532_00005.perseus.localdomain"
    val nameWithFlags = "1358675765.03532_10005.perseus.localdomain:2,RS"

    val fileName = MaildirFilename.parseFrom(name)
    val fileNameWithFlags = MaildirFilename.parseFrom(nameWithFlags)


    fileName.name should equal(name)
    fileNameWithFlags.name should equal(nameWithFlags)
  }

  "A MaildirFilename" should "is cloneable without flags" in {
    val nameWithFlags = "1358675765.03532_10005.perseus.localdomain:2,RS"

    val fileNameWithFlags = MaildirFilename.parseFrom(nameWithFlags)
    val clonedFileName = fileNameWithFlags.cloneWithoutFlags

    clonedFileName.name should equal(fileNameWithFlags.uniq)
  }

  def hostname = InetAddress.getLocalHost.getHostName

}
