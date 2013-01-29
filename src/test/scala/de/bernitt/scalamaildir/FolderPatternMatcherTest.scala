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

class FolderPatternMatcherTest extends FlatSpec with ShouldMatchers {

  "A FolderPatternMatcher" should "match every maildir folder with wildcard '*'" in {
    val matcher = new FolderPatternMatcher("*")

    matcher.matchesEncoded("INBOX") should be(true)
    matcher.matchesEncoded(".foobar") should be(true)
    matcher.matchesEncoded(".foo.bar") should be(true)
    matcher.matchesEncoded(".&AOkfoo.bar") should be(true)
  }

  "A FolderPatternMatcher" should "match inbox and sub folders with wildcard '*'" in {
    val matcher = new FolderPatternMatcher("IN*")

    matcher.matchesEncoded("INBOX") should be(true)
    matcher.matchesEncoded(".INtest") should be(true)
    matcher.matchesEncoded(".foobar") should be(false)
  }

  "A FolderPatternMatcher" should "match no sub folders with wildcard '%'" in {
    val matcher = new FolderPatternMatcher("%")

    matcher.matchesEncoded("INBOX") should be(true)
    matcher.matchesEncoded(".foo") should be(true)
    matcher.matchesEncoded(".foo.bar") should be(false)
  }

  "A FolderPatternMatcher" should "match only subfolders with bar " in {
    val matcher = new FolderPatternMatcher("%/bar")

    matcher.matchesEncoded("INBOX") should be(false)
    matcher.matchesEncoded(".foo") should be(false)
    matcher.matchesEncoded(".foo.bar") should be(true)
    matcher.matchesEncoded(".test.bar") should be(true)
  }

  "A FolderPatternMatcher" should "match mixed patterns" in {
    val matcher = new FolderPatternMatcher("foo*/b%r")

    matcher.matchesEncoded("INBOX") should be(false)
    matcher.matchesEncoded(".foo.bar") should be(true)
    matcher.matchesEncoded(".foo.test.bar") should be(true)
    matcher.matchesEncoded(".foo.bar.test") should be(false)
  }

}