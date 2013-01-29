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

import javax.mail.Flags
import java.net.InetAddress
import java.util.concurrent.atomic.AtomicInteger
import javax.mail.Flags.Flag
import collection.mutable.ArrayBuffer

object MaildirFilename {
  val supoortedFlags = Set(Flag.ANSWERED, Flag.DELETED, Flag.DRAFT, Flag.FLAGGED, Flag.SEEN)
  var count = new AtomicInteger()

  def create(): MaildirFilename = {
    val timestamp = System.currentTimeMillis / 1000
    val deliveryIdentifier = zeroPadTo(processId, 5) + "_" + zeroPadTo(count.incrementAndGet(), 5)
    val hostname = localHostName


    new MaildirFilename(timestamp, deliveryIdentifier, hostname, new Flags())
  }

  def parseFrom(fileName: String): MaildirFilename = {
    val matcher = "([0-9]+)[.]([^.]+)[.]([^:]+)(:2,([DFRST]*))?".r.pattern.matcher(fileName)
    if (matcher.matches()) {
      val timestamp = matcher.group(1).toLong
      val deliveryIdentifier = matcher.group(2)
      val hostname = matcher.group(3)
      val flags = new Flags()

      if (matcher.group(5) != null) {
        matcher.group(5).toCharArray.foreach(_ match {
          case 'D' => flags.add(Flag.DRAFT)
          case 'F' => flags.add(Flag.FLAGGED)
          case 'R' => flags.add(Flag.ANSWERED)
          case 'S' => flags.add(Flag.SEEN)
          case 'T' => flags.add(Flag.DELETED)
        })
      }
      new MaildirFilename(timestamp, deliveryIdentifier, hostname, flags)
    } else {
      throw new IllegalArgumentException("Not able to parse file name" + fileName)
    }
  }

  private def zeroPadTo(number: Int, digits: Int) = String.format("%0" + digits + "d", int2Integer(number))

  private def localHostName = InetAddress.getLocalHost.getHostName

  private def processId = Math.abs((Thread.currentThread().hashCode() % 65534 + 1))
}

class MaildirFilename private(timestamp: Long, deliveryIdentifier: String, hostname: String, val flags: Flags) {

  def uniq = timestamp.toString + "." + deliveryIdentifier + "." + hostname

  def info = buildFlags

  def name = if (hasFlags) uniq + ":" + info else uniq

  override def toString: String = name

  override def hashCode(): Int = name.hashCode

  override def equals(other: Any): Boolean = other match {
    case other: MaildirFilename => other.name == name
    case _ => false
  }

  def setFlag(flag: Flag) {
    flags.add(flag)
  }

  def removeFlag(flag: Flag) {
    flags.remove(flag)
  }

  def addFlags(newFlags: Flags) {
    flags.add(newFlags)
  }

  def cloneWithoutFlags = new MaildirFilename(timestamp, deliveryIdentifier, hostname, new Flags())

  private def hasFlags = flags.getSystemFlags.length > 0

  private def buildFlags = {
    if (hasFlags) {
      val systemFlags = supportedSystemFlags
      val buffer = new ArrayBuffer[String]
      systemFlags.foreach(flag => buffer.append(flag match {
        case Flag.SEEN => "S"
        case Flag.ANSWERED => "R"
        case Flag.DELETED => "T"
        case Flag.DRAFT => "D"
        case Flag.FLAGGED => "F"
        case Flag.RECENT => "" // not added as file name flag
        case _ => throw new IllegalArgumentException("unexpected flag ")
      }))

      "2," + buffer.sortWith(_ < _).mkString
    } else {
      ""
    }
  }

  private def supportedSystemFlags = flags.getSystemFlags.toSet.intersect(MaildirFilename.supoortedFlags)

}
