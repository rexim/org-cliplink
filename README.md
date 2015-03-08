[![MELPA badge](http://melpa.org/packages/org-cliplink-badge.svg)](http://melpa.org/#/org-cliplink) [![Build Status](https://travis-ci.org/rexim/org-cliplink.svg?branch=master)](https://travis-ci.org/rexim/org-cliplink)

# org-cliplink #

A simple command that takes a URL from the clipboard and inserts an
org-mode link with a title of a page found by the URL into the current
buffer.

This code was a part of my Emacs config almost a year. I decided to
publish it as a separate package in case someone needs this feature
too.

# Usage #

Bind `org-cliplink` function to something. For example, put this line
in your init file:

    (global-set-key (kbd "C-x p i") 'org-cliplink)

Then copy any http/https URL to the clipboard, switch to the Emacs
window and hit `C-x p i`.

# Dependencies #

## Windows ##

- GnuTLS — if you use Emacs installation from the official GNU FTP
  server — ftp://ftp.gnu.org/gnu/emacs/windows/ — you may simply
  download the latest version of GnuTLS from
  ftp://ftp.gnutls.org/gcrypt/gnutls/w32/ and copy the content of the
  downloaded archive to the emacs installation folder.

# Custom commands #

It is possible to create your own `org-cliplink`-like commands with
the `org-cliplink-retrieve-title` function. For example:

    (org-cliplink-retrieve-title
     "https://google.com/"
     `(lambda (url title)
        (if title
            (message "%s has title %s" url title)
          (message "%s doesn't have title" url))))

I use it for making org-mode tasks from URLs. Here is some code from
my Emacs config:

    (defun rc/cliplink-task ()
      (interactive)
      (org-cliplink-retrieve-title
       (substring-no-properties (current-kill 0))
       '(lambda (url title)
          (insert (concat "* TODO " title
                          "\n  [[" url "][" title "]]")))))

     (global-set-key (kbd "C-x p t") 'rc/cliplink-task)

So when I browse the Web and see an interesting article that I want to
read later I just copy its URL to the clipboard, switch to my org-mode
files with personal notes and hit `C-x p t`.

For more information see `C-h f org-cliplink-retrieve-title RET`.

# Bugs #

https://github.com/rexim/org-cliplink/issues

# Contribution #

This command doesn't handle some cases (like different encodings) but
I do my best to improve it. If you find this code useful and want to
make a contribution feel free to make a pull request. :)

Thanks.
