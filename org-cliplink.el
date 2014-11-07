;;; org-cliplink.el --- a simple command that takes a URL from the clipboard and inserts an org-mode link with a title of a page found by the URL into the current buffer

;; Copyright (C) 2014 Alexey Kutepov a.k.a rexim

;; Author: Alexey Kutepov <reximkut@gmail.com>
;; URL: http://github.com/rexim/org-cliplink
;; Version: 0.1.1

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Usage:
;;
;; Bind `org-cliplink` function to something. For example, put
;; this line in your init file:
;;   (global-set-key (kbd "C-x p i") 'org-cliplink)
;;
;; Then copy any http/https URL to the clipboard, switch to
;; the Emacs window and hit `C-x p i`.

;;; Commentary:
;; 
;; A simple command that takes a URL from the clipboard and inserts an
;; org-mode link with a title of a page found by the URL into the
;; current buffer
;; 
;; This code was a part of my Emacs config almost a year. I decided to
;; publish it as a separate package in case someone needs this feature
;; too.

(defun straight-string (s)
  (mapconcat '(lambda (x) x) (split-string s) " "))

(defun extract-title-from-html (html)
  (let ((start (string-match "<title>" html))
        (end (string-match "</title>" html))
        (chars-to-skip (length "<title>")))
    (if (and start end (< start end))
        (substring html (+ start chars-to-skip) end)
      nil)))

(defun prepare-cliplink-title (title)
  (let ((replace-table '(("\\[" . "{")
                         ("\\]" . "}")
                         ("&mdash;" . "—")
                         ("&#39;" . "'")))
        (max-length 77)
        (result (straight-string title)))
    (dolist (x replace-table)
      (setq result (replace-regexp-in-string (car x) (cdr x) result)))
    (when (> (length result) max-length)
      (setq result (concat (substring result 0 max-length) "...")))
    result))

(defun perform-cliplink (buffer url content)
  (let* ((decoded-content (decode-coding-string content 'utf-8))
         (title (extract-title-from-html decoded-content)))
    (with-current-buffer buffer
      (if title
          (insert (format "[[%s][%s]]" url (prepare-cliplink-title title)))
        (insert (format "[[%s]]" url))))))

;;;###autoload
(defun org-cliplink ()
  (interactive)
  (let ((dest-buffer (current-buffer))
        (url (substring-no-properties (current-kill 0))))
    (url-retrieve
     url
     `(lambda (s)
        (perform-cliplink ,dest-buffer ,url
                          (buffer-string))))))

;;; org-cliplink.el ends here.
