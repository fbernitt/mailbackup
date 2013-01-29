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


object MaildirBase64Decoder {
  def decode(str: String): String = {
    val writer = new CharArrayWriter()
    val decoder = new MaildirBase64Decoder(writer)

    decoder.write(str)

    writer.toString

  }
}

class MaildirBase64Decoder(out: Writer) extends Base64Util {
  val escapeChars = new ArrayBuffer[Char]
  var escapeMode = false

  def write(str: String) {
    str.foreach {
      c: Char =>
        write(c)
    }
  }


  private def write(chr: Char) {
    if (escapeMode) {
      writeEscapeChar(chr)
    } else if (isPlainChar(chr)) {
      writeToOut(chr)
    } else if (isEscapeChar(chr)) {
      startEscapeSequence()
    } else {
      throw new RuntimeException("Unexpected character: [" + chr + "]")
    }
  }

  private def writeEscapeChar(chr: Char) {
    if (isFinishEscapeChar(chr)) {
      finishEscapeSequence()
    } else {
      if (TARGET_CHARS.contains(chr)) {
        addSpecialChar(chr)
      } else {
        throw new RuntimeException("Unexpected escape character: [" + chr + "]")
      }
    }
  }

  private def addSpecialChar(chr: Char) {
    escapeChars.append(chr)
  }

  private def startEscapeSequence() {
    escapeMode = true
  }


  private def finishEscapeSequence() {
    if (isSpecialSequenceForAmpersand) {
      writeToOut('&')
    } else {
      new Base64Decoder().decodeChars(escapeChars.toArray).foreach(writeToOut(_))
    }
    escapeChars.clear()
    escapeMode = false
  }

  private def isSpecialSequenceForAmpersand: Boolean = {
    escapeChars.isEmpty
  }


  private def writeToOut(chr: Char) {
    out.write(chr)
  }

  private class Base64Decoder() {
    def decodeChars(source: Array[Char]): Array[Char] = {
      val sixBits = escapeCharsTo6BitArray(source)
      val bytes = sixBitArrayToByteArray(sixBits)
      bytesToChars(bytes)
    }

    private def bytesToChars(bytes: Array[Byte]): Array[Char] = {
      val chars = new ArrayBuffer[Char]()

      var pos = 0
      while (pos < bytes.size - 1) {
        val chr = (((bytes(pos) << 8) | bytes(pos + 1) & 0xFF) & 0xFFFF).toChar
        chars.append(chr)
        pos = pos + 2
      }

      chars.toArray
    }

    private def sixBitArrayToByteArray(sixBits: Array[Byte]): Array[Byte] = {
      val target: ArrayBuffer[Byte] = new ArrayBuffer[Byte]
      var acc = ((sixBits(0) & 0x3F) << 6) | (sixBits(1) & 0x3F)
      var bits = 12
      var idx = 2

      while (idx < sixBits.length && notAllBitsConsumed(bits)) {
        if (bits >= 8) {
          acc = top8BitsTo(acc, bits, target)
          bits = eightBitsConsumed(bits)
        }
        if (bits < 8 && idx < sixBits.size) {
          acc = next6BitsToAcc(acc, sixBits(idx), bits)
          bits = sixBitsAdded(bits)
          idx = idx + 1
        }
      }

      if (notAllBitsConsumed(bits)) {
        acc = next6BitsToAcc(acc, 0, bits)
        bits = sixBitsAdded(bits)
        acc = top8BitsTo(acc, bits, target)
      }
      target.toArray
    }

    private def next6BitsToAcc(acc: Int, byte: Byte, bits: Int): Int = {
      (acc << 6 | byte & 0xFF) & bitMaskWithBits(bits + 6)
    }

    private def eightBitsConsumed(bits: Int): Int = bits - 8

    private def sixBitsAdded(bits: Int): Int = bits + 6


    private def top8BitsTo(acc: Int, bits: Int, bytes: ArrayBuffer[Byte]): Int = {
      bytes.append(((acc >>> (bits - 8)) & 0xFF).toByte)

      acc & bitMaskWithBits(bits - 8)
    }

    private def bitMaskWithBits(bits: Int): Int = {
      if (bits <= 0) {
        0
      } else {
        1 + (bitMaskWithBits(bits - 1) << 1)
      }
      //      var mask = 0
      //      for (i <- 1 to bits) {
      //        mask = (mask << 1) | 1
      //      }
      //      mask
    }

    private def notAllBitsConsumed(bits: Int) = bits > 0

    private def escapeCharsTo6BitArray(chars: Array[Char]): Array[Byte] = {
      for (ch <- chars) yield TARGET_CHARS.indexOf(ch).toByte
    }

    def toBinary(i: Int, digits: Int = 8) = String.format("%" + digits + "s", i.toBinaryString).replace(' ', '0')
  }

}
