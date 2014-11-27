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

# Bugs #

https://github.com/rexim/org-cliplink/issues

# Contribution #

This command doesn't handle some cases (like different encodings and
compressed responses) but I do my best to improve it. If you find this
code useful and want to make a contribution feel free to make a pull
request. :)

Thanks.
