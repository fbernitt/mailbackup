package de.bernitt.mailbackup

import org.scalatest.{BeforeAndAfterEach, FlatSpec}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.mock.MockitoSugar
import javax.mail.{FetchProfile, Message, Folder}
import org.mockito.Mockito._
import org.mockito.Matchers.{eq => the, _}

class FolderMessageIdFetcherTest extends FlatSpec with ShouldMatchers with BeforeAndAfterEach with MockitoSugar {

  val folder = mock[Folder]
  val message = mock[Message]

  override protected def beforeEach() {
    reset(folder)
    reset(message)
  }

  "A FolderMessageIdFetcher" should "fetch message ids from closed folder in readonly mode" in {
    //given
    val msgs = Array(message)
    when(folder.getMessages).thenReturn(msgs)
    when(message.getHeader("Message-Id")).thenReturn(Array("first"))

    //when
    val msgIds = new FolderMessageIdFetcher(folder).messageIds

    //then
    msgIds should equal(Array("first"))
    verify(folder).open(Folder.READ_ONLY)
    verify(folder).fetch(the(msgs), isA(classOf[FetchProfile]))
    verify(folder).close(false)
  }

  "A FolderMessageIdFetcher" should "should not reopen an already opened folder" in {
    val msgs = Array(message)
    when(folder.isOpen).thenReturn(true)
    when(folder.getMessages).thenReturn(msgs)
    when(message.getHeader("Message-Id")).thenReturn(Array("first"))

    //when
    val msgIds = new FolderMessageIdFetcher(folder).messageIds

    //then
    msgIds should equal(Array("first"))
    verify(folder, never()).open(Folder.READ_ONLY)
    verify(folder).fetch(the(msgs), isA(classOf[FetchProfile]))
    verify(folder, never()).close(false)

  }

}
