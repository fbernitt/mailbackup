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

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import java.io.CharArrayWriter

class MaildirBase64EncoderTest extends FlatSpec with ShouldMatchers {

  "A MaildirBase64Encoder" should "encode '&' as '&-'" in {
    base64("&") should equal("&-")
  }

  "A MaildirBase64Encoder" should "encode standard ascii chars 0x0020 to 0x007F as themselfs" in {
    val result = base64(" !\"#$%'()*+,-0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\u007F")
    result should equal(" !\"#$%'()*+,-0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\u007F")
  }

  "A MaildirBase64Encoder" should "encode other single chars as base 64" in {
    base64('\u0000') should equal("&AAA-")
    base64('\u0001') should equal("&AAE-")
    base64('\u0002') should equal("&AAI-")
    base64('\u00e9') should equal("&AOk-")
  }

  "A MaildirBase64Encoder" should "encode multiple chars" in {
    base64("\u0000\u0000\u0000") should equal("&AAAAAAAA-")
    base64("\u00e9\u00e9") should equal("&AOkA6Q-")
    base64("\u0001\u0019\u0321") should equal("&AAEAGQMh-")
    base64("\u0001\u0019\u0321\u3210") should equal("&AAEAGQMhMhA-")
  }

  "A MaildirBase64Encoder" should "encode resume as in http://www.courier-mta.org/maildir.html described" in {
    base64("R\u00e9sum\u00e9") should equal("R&AOk-sum&AOk-")
  }

  def base64(char: Char): String = {
    val writer = new CharArrayWriter()

    new MaildirBase64Encoder(writer).encodeSingleChar(char)

    writer.toString
  }

  def base64(str: String): String = MaildirBase64Encoder.encode(str)

}
