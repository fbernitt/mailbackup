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
import java.io.File

class MaildirFolderNameTest extends FlatSpec with ShouldMatchers {

  "A MaildirFolderName" should "accept a file name" in {
    val folderName = new MaildirFolderName("foo")

    folderName.name should equal("foo")
  }

  "A MaildirFolderName" should "add . as prefix for maildir name" in {
    val folderName = new MaildirFolderName("foo")

    folderName.maildirName should equal(".foo")
  }

  "A MaildirFolderName" should "should replace path separator with ." in {
    val folderName = new MaildirFolderName("foo" + File.separatorChar + "bar")

    folderName.maildirName should equal(".foo.bar")
  }

  "A MaildirFolderName" should "should return only last part as name" in {
    val folderName = new MaildirFolderName("foo" + File.separatorChar + "bar")

    folderName.name should equal("bar")
  }

  "A MaildirFolderName" should "should return full path for full name" in {
    val name = "foo" + File.separatorChar + "bar"
    val folderName = new MaildirFolderName(name)

    folderName.fullName should equal(name)
  }

  "A MaildirFolderName" should "handle INBOX as special folder name" in {
    val inboxFolder = new MaildirFolderName("INBOX")

    inboxFolder.name should equal("INBOX")
    inboxFolder.maildirName should equal(".")
  }

  "A MaildirFolderName" should "handle base64 encoding" in {
    val simpleFolder = new MaildirFolderName("\u00e9simple\u00e9")
    val childFolder = new MaildirFolderName("\u00e9parent\u00eA" + File.separatorChar + "\u00e9child\u002e\u00eA")

    simpleFolder.name should equal("\u00e9simple\u00e9")
    simpleFolder.maildirName should equal(".&AOk-simple&AOk-")

    //childFolder.name should equal ("\u00e9child\u00eA")
    childFolder.maildirName should equal(".&AOk-parent&AOo-.&AOk-child&AC4A6g-")
  }

  "A MaildirFolderName" should "allow to build relative path" in {
    val inboxFolder = new MaildirFolderName("INBOX")
    val foobarFolder = new MaildirFolderName("foobar")
    val barfooFolder = new MaildirFolderName("foobar/barfoo")
    val tmp = new File("/tmp")

    inboxFolder.pathRelativeTo(tmp) should equal(tmp)
    foobarFolder.pathRelativeTo(tmp) should equal(new File(tmp, ".foobar"))
    barfooFolder.pathRelativeTo(tmp) should equal(new File(tmp, ".foobar.barfoo"))
  }

  "A MaildirFolderName" should "allow to check for possible parents" in {
    val topFolder = new MaildirFolderName("top")
    val childFolder = new MaildirFolderName("top/child")

    topFolder.hasParent should equal(false)
    childFolder.hasParent should equal(true)
  }

  "A MaiildirFolder" should "return its parent folder" in {
    val childFolder = new MaildirFolderName("top/child")
    val topFolder = new MaildirFolderName("top")

    childFolder.parent should equal(topFolder)
  }
}
