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

package de.bernitt.scalamaildir.io

import java.io.FilterOutputStream
import java.io.OutputStream

object LocalPlatformNewlineFilterOutputStream {
  val newLine: Array[Byte] = lineSeparatorBytes


  private def lineSeparatorBytes: Array[Byte] = {
    val sep = System.getProperty("line.separator")
    val newLineChar = if (separatorDefined(sep)) sep else "\n"

    newLineChar.getBytes("ISO-8859-1")
  }

  private def separatorDefined(sep: String): Boolean = sep == null || sep.length == 0
}

class LocalPlatformNewlineFilterOutputStream(out: OutputStream) extends FilterOutputStream(out) {

  private var lastByte = -1

  override def write(byte: Int) {
    if (byte == '\r') {
      out.write(LocalPlatformNewlineFilterOutputStream.newLine)
    } else if (byte == '\n') {
      if (lastByte != '\r') {
        out.write(LocalPlatformNewlineFilterOutputStream.newLine)
      }
    } else {
      out.write(byte)
    }
    lastByte = byte
  }

  override def write(bytes: Array[Byte]) {
    write(bytes, 0, bytes.length)
  }

  override def write(bytes: Array[Byte], offset: Int, length: Int) {
    for (i <- 0 to length - 1) {
      write(bytes(offset + i))
    }
  }
}
