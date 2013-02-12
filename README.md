mailbackup
==========

A scala library and tool to backup remote IMAP accounts to a local Maildir.

Currently work in progress, Maildir JavaMail store nearly finished.

Maildir
-------
[Wikipedia Maildir](http://en.wikipedia.org/wiki/Maildir)

Compile and Install
-------------------

To build this project you need [sbt, the scala build tool](https://github.com/sbt/sbt).

    sbt clean assembly


To install the mailbackup, simply copy target/{mail-backup,mailbackup-assembly-1.0.jar} to ~/bin

    cp target/{mail-backup,mailbackup-assembly-1.0.jar} ~/bin

Usage
-----

    # backup account imapuser on host some.imap.host to /tmp/mailbackup

    mkdir -p /tmp/mailbackup/{new,tmp,cur}
    target/mail-backup imap://imapuser@some.imap.host/ /tmp/mailbackup/

