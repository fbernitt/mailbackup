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

import java.io.{CharArrayWriter, Writer}
import collection.mutable.ArrayBuffer


object MaildirBase64Encoder {

  def encode(str: String): String = {
    val writer = new CharArrayWriter()
    val encoder = new MaildirBase64Encoder(writer)

    encoder.write(str)

    writer.toString
  }
}

class MaildirBase64Encoder(out: Writer) extends Base64Util {
  val specialChars = new ArrayBuffer[Char]

  def encodeSingleChar(chr: Char) {
    write(chr.toString)
  }

  def write(char: Char) {
    if (isPlainChar(char)) {
      finishEncoderIfNecessary()
      writeToOut(char)
    } else if (isEscapeChar(char)) {
      finishEncoderIfNecessary()
      writeEscapeCharReplacement()
    } else {
      addSpecialChar(char)
    }
  }

  def write(str: String) {
    str.foreach {
      c: Char =>
        write(c)
    }

    finishEncoderIfNecessary()
  }

  private def encodeNecessary = specialChars.length > 0

  private def finishEncoderIfNecessary() {
    if (encodeNecessary) {
      out.write(encodeSpecialChars())
    }
  }

  private def encodeSpecialChars(): String = {
    val encoder = new Base64Encoder()
    val encodedChars = encoder.encodeChars(specialChars.toArray)
    specialChars.clear()

    encodedChars.mkString
  }

  private def writeEscapeCharReplacement() {
    out.write("&-");
  }

  private def addSpecialChar(chr: Char) {
    specialChars.append(chr)
  }

  private def writeToOut(chr: Char) {
    out.write(chr)
  }


  private class Base64Encoder {

    def encodeChars(source: Array[Char]): Array[Char] = {
      val bytes = byteArrayTo6BitArray(charToBytes(source))

      val writer = new CharArrayWriter()
      writer.write("&")

      bytes.foreach {
        b: Byte => writer.append(TARGET_CHARS(b))
      }

      writer.write("-")

      writer.toCharArray
    }

    private def charToBytes(chars: Array[Char]): Array[Byte] = {
      val buffer = new ArrayBuffer[Byte](chars.length * 2)
      chars.foreach {
        char =>
          buffer.append(((char.toInt >> 8) & 0xFF).toByte)
          buffer.append((char.toInt & 0xFF).toByte)
      }

      buffer.toArray
    }

    private def top6BitsTo(word: Int, sixBits: ArrayBuffer[Byte]): Int = {
      sixBits.append(((word >>> 10) & 0x3F).toByte)

      (word << 6) & 0xFFFF
    }

    private def nextByteToAcc(acc: Int, byte: Byte, bits: Int): Int = {
      (((byte & 0xFF) << (8 - bits)) | acc) & 0xFFFF
    }

    private def sixBitsConsumed(bits: Int) = bits - 6

    private def oneByteAdded(bits: Int) = bits + 8

    private def notAllBitsConsumed(bits: Int) = bits > 0

    private def byteArrayTo6BitArray(source: Array[Byte]): Array[Byte] = {
      val target: ArrayBuffer[Byte] = new ArrayBuffer[Byte](4)
      var acc = (source(0).toInt << 8) & 0xFF00 | (source(1) & 0x00FF)
      var bits = 16
      var idx = 2

      while (idx <= source.size && notAllBitsConsumed(bits)) {
        acc = top6BitsTo(acc, target)
        bits = sixBitsConsumed(bits)
        if (bits < 6 && idx < source.size) {
          acc = nextByteToAcc(acc, source(idx), bits)
          bits = oneByteAdded(bits)
          idx = idx + 1
        }
      }

      if (notAllBitsConsumed(bits)) {
        // last bits to buffer
        top6BitsTo(acc, target)
      }
      target.toArray
    }

  }

}
