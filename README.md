Simple Web Server
=================

What it is
----------

`sws` is a self-contained web server that serves files which runs on Linux, Windows and (untested) Mac OS X.  Once built, 
the executable should have no dependencies, e.g., it does not require OpenSSL.  Convenience and security are the
main goals.  It has no config files, and only a few, if any, easily provided command line parameters should be necessary.
If convenience and security conflict, I'm willing to sacrifice a little convenience for security, but only a little.
Often such conflicts are largely resolvable.  For example, requiring a password and using TLS improve security, but
making a password or a certificate are inconvenient, so `sws` can generate these.

### Use-case 1: Large file transfer
    
You want to send a large file to someone.  You browse to the directory containing it, type "`sws`", and give them
your public IP.  They browse to it and download.  Maybe they are the ones sending the file, but aren't "technical".
You browse to an empty directory, type "`sws -w`" or "`sws -U`", and give them your public IP.  They browse to it and upload.

In reality, you need to figure out what your public IP is and open a port in your firewall.  `sws` will currently
use Google's STUN server to attempt to figure out your public IP.

### Use-case 2: Client-side code demo/development

You build an [unhosted](https://unhosted.org/) web application or you mock out AJAX responses.  You can do some
simple testing by running "`sws -d`".  (Admittedly, using a file URI will probably work pretty well too, though maybe
not so much for mocked POST requests...)  You want to show a friend.  Just make that "`sws`".

### Use-case 3: Instant read-only Git hosting

```bash
git clone --bare /path/to/myrepo ~/public/myrepo.git
cd ~/public/myrepo.git
mv hooks/post-update.sample hooks/post-update
chmod a+x hooks/post-update
git update-server-info
sws --no-compress -H example.com ~/public/
```

Elsewhere:
```bash
git -c http.sslVerify=false clone https://example.com:3000/myrepo.git
```

This would be even more instant with darcs.

What it isn't
-------------

This is not an app server.  It reads and writes files, and that's all it will ever do.  There is no way to add
code to it.  The Haskell ecosystem has plenty of good web frameworks.  This is not one of them.  It's an
application, not a framework.  (Well... you *could* do something with named pipes and/or interesting file systems...
but you really shouldn't.)
