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

package de.bernitt.mailbackup

import org.scalatest.{BeforeAndAfterEach, FlatSpec}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.mock.MockitoSugar
import javax.mail.{MessagingException, Message, Folder, Store}
import org.mockito.Mockito._
import org.mockito.Matchers.{eq => the, any, anyInt}
import scala.Array

class MailBackupTest extends FlatSpec with ShouldMatchers with BeforeAndAfterEach with MockitoSugar {

  val srcStore = mock[Store]
  val dstStore = mock[Store]

  val srcDefaultFolder = mock[Folder]

  val srcFolder = mock[Folder]
  val dstFolder = mock[Folder]

  val srcMessage = createMessageMock("msgId:1")

  override protected def beforeEach() {
    reset(srcStore)
    reset(dstStore)
    reset(srcFolder)
    reset(dstFolder)

    when(srcStore.getDefaultFolder).thenReturn(srcDefaultFolder)
    when(dstStore.getFolder("testFolder")).thenReturn(dstFolder)
    when(srcFolder.getFullName).thenReturn("testFolder")
  }

  "A MailBackup" should "create dst folder and add missing message" in {
    //given
    prepareSingleSourceFolderWithMessages(Array(srcMessage))
    when(dstFolder.getMessages).thenReturn(Array[Message]())

    //when
    new MailBackup(srcStore, dstStore).backup()

    //then
    assertSrcFolderOpenedReadonly()
    assertDstFolderIsCreated()
    assertFoldersAreClosedWithoutExpunge()
    assertMessagesAdded(Array(srcMessage))
  }

  "A MailBackup" should "should not create dst folder if it already exists" in {
    //given
    prepareSingleSourceFolderWithMessages(Array(srcMessage))
    prepareDestinationFolderWith(Array[Message]())

    when(dstFolder.exists()).thenReturn(true)

    //when
    new MailBackup(srcStore, dstStore).backup()

    //then
    verify(dstFolder, never()).create(Folder.HOLDS_FOLDERS | Folder.HOLDS_MESSAGES)
    assertFoldersAreClosedWithoutExpunge()
    assertMessagesAdded(Array(srcMessage))
  }

  "A MailBackup" should "not create dst folder it's not possible to open src folder" in {
    //given
    prepareSingleSourceFolderWithMessages(Array(srcMessage))
    when(srcFolder.open(anyInt())).thenThrow(new MessagingException("Some error occurred"))

    //when
    new MailBackup(srcStore, dstStore).backup()

    //then
    verify(dstFolder, never()).create(anyInt())
  }

  "A MailBackup" should "should only add missing messages" in {
    //given
    val existingMsg = createMessageMock("msgId:2")

    prepareSingleSourceFolderWithMessages(Array(srcMessage, existingMsg))
    prepareDestinationFolderWith(Array(existingMsg))
    when(dstFolder.exists()).thenReturn(true)

    //when
    new MailBackup(srcStore, dstStore).backup()

    //then
    assertMessagesAdded(Array(srcMessage))
  }

  "A MailBackup" should "should not add anything in dry run mode" in {
    //given
    prepareSingleSourceFolderWithMessages(Array(srcMessage))
    prepareDestinationFolderWith(Array[Message]())
    when(dstFolder.exists()).thenReturn(true)

    //when
    new MailBackup(srcStore, dstStore, true).backup()

    //then
    assertSrcFolderOpenedReadonly()
    assertDstFolderOpendReadWrite()
    assertFoldersAreClosedWithoutExpunge()

    verify(dstFolder, never()).appendMessages(any())
  }

  "A MailBackup" should "add nothing if all message already in dst folder" in {
    //given
    prepareSingleSourceFolderWithMessages(Array(srcMessage))
    prepareDestinationFolderWith(Array(srcMessage))
    when(dstFolder.exists()).thenReturn(true)

    //when
    new MailBackup(srcStore, dstStore).backup()

    //then
    assertSrcFolderOpenedReadonly()
    assertFoldersAreClosedWithoutExpunge()
    verify(dstFolder, never()).appendMessages(any())
  }

  "A MailBackup" should "iterate over all folders" in {
    //given
    val anotherSrcFolder = mock[Folder]
    when(anotherSrcFolder.getFullName).thenReturn("anotherFolder")
    val anotherDstFolder = mock[Folder]
    when(dstStore.getFolder("anotherFolder")).thenReturn(anotherDstFolder)
    when(anotherDstFolder.getFullName).thenReturn("anotherFolder")
    when(anotherSrcFolder.getMessages).thenReturn(Array(srcMessage))
    when(anotherDstFolder.getMessages).thenReturn(Array[Message]())

    when(srcDefaultFolder.list("*")).thenReturn(Array(srcFolder, anotherSrcFolder))
    when(srcFolder.getMessages).thenReturn(Array(srcMessage))
    prepareDestinationFolderWith(Array(srcMessage))
    when(dstFolder.exists()).thenReturn(true)

    //when
    new MailBackup(srcStore, dstStore).backup()

    //then
    assertSrcFolderOpenedReadonly()
    assertFoldersAreClosedWithoutExpunge()
    verify(dstFolder, never()).appendMessages(any())
    verify(anotherDstFolder).appendMessages(the(Array(srcMessage)))
  }

  "A MailBackup" should "continue with next folder if exception occurs" in {
    //given
    val anotherSrcFolder = mock[Folder]
    when(anotherSrcFolder.getFullName).thenReturn("anotherFolder")
    val anotherDstFolder = mock[Folder]
    when(dstStore.getFolder("anotherFolder")).thenReturn(anotherDstFolder)
    when(anotherDstFolder.getFullName).thenReturn("anotherFolder")
    when(anotherSrcFolder.getMessages).thenReturn(Array(srcMessage))
    when(anotherDstFolder.getMessages).thenReturn(Array[Message]())

    when(srcDefaultFolder.list("*")).thenReturn(Array(srcFolder, anotherSrcFolder))
    when(srcFolder.open(anyInt())).thenThrow(new MessagingException("Some exception"))

    //when
    new MailBackup(srcStore, dstStore).backup()

    //then
    verify(dstFolder, never()).appendMessages(any())
    verify(anotherDstFolder).appendMessages(the(Array(srcMessage)))
  }

  def createMessageMock(messageId: String): Message = {
    val msg = mock[Message]
    when(msg.getHeader("Message-Id")).thenReturn(Array(messageId))
    msg
  }

  private def prepareSingleSourceFolderWithMessages(msgs: Array[Message]) {
    when(srcDefaultFolder.list("*")).thenReturn(Array(srcFolder))
    when(srcFolder.getMessages).thenReturn(msgs)
  }

  private def prepareDestinationFolderWith(msgs: Array[Message]) {
    when(dstFolder.getMessages).thenReturn(msgs)
  }

  def assertMessagesAdded(array: Array[Message]) {
    verify(dstFolder).appendMessages(the(array))
  }

  def assertDstFolderIsCreated() {
    verify(dstFolder).create(Folder.HOLDS_FOLDERS | Folder.HOLDS_MESSAGES)
  }

  private def assertSrcFolderOpenedReadonly() {
    verify(srcFolder, atLeastOnce()).open(Folder.READ_ONLY)
    verify(srcFolder, never()).open(Folder.READ_WRITE)
  }

  private def assertDstFolderOpendReadWrite() {
    verify(dstFolder, atLeastOnce()).open(Folder.READ_WRITE)
  }

  private def assertFoldersAreClosedWithoutExpunge() {
    verify(srcFolder, atLeastOnce()).close(false)
    verify(dstFolder, atLeastOnce()).close(false)
    verify(srcFolder, never()).close(true)
  }
}
