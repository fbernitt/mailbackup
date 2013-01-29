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


class MaildirBase64DecoderTest extends FlatSpec with ShouldMatchers {

  "A MaildirBase64Decoder" should "decode standard ascii chars 0x0020 to 0x007F as themselfs" in {
    val result = decode(" !\"#$%'()*+,-0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\u007F")

    result should equal(" !\"#$%'()*+,-0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\u007F")
  }

  "A MaildirBase64Decoder" should "decode special escape sequence &- as &" in {
    decode("&-") should equal("&")
  }

  "A MaildirBase64Decoder" should "decode encoded sequences" in {
    decode("&AAA-") should equal("\u0000")
    decode("&AAE-") should equal("\u0001")
    decode("&AAI-") should equal("\u0002")
    decode("&AOk-") should equal("\u00e9")
  }

  "A MaildirBase64Decoder" should "decode encoded multi character sequences" in {
    decode("&AAAAAAAA-") should equal("\u0000\u0000\u0000")
    decode("&AOkA6Q-") should equal("\u00e9\u00e9")
    decode("&AAEAGQMh-") should equal("\u0001\u0019\u0321")
  }

  "A MaildirBase64Encoder" should "decode resume as in http://www.courier-mta.org/maildir.html described" in {
    decode("R&AOk-sum&AOk-") should equal("R\u00e9sum\u00e9")
  }

  private def decode(str: String): String = MaildirBase64Decoder.decode(str)
}
