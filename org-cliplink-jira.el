;;; org-cliplink-jira.el --- insert org-mode link for URL from the clipboard

;; Copyright (C) 2014 Alexey Kutepov a.k.a rexim

;; Author: Alexey Kutepov <reximkut@gmail.com>
;; Maintainer: Alexey Kutepov <reximkut@gmail.com>
;; URL: http://github.com/rexim/org-cliplink
;; Version: 0.2

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

;;; Commentary:
;; 
;; This library adds JIRA support for org-cliplink

(require 'json)
(require 'org-cliplink)

(defun org-cliplink-jira-extract-summary-from-current-buffer ()
  (let* ((response (org-cliplink-parse-response))
         (header (car response))
         (json-content (json-read-from-string
                        (if (string= "gzip" (cdr (assoc "Content-Encoding" header)))
                            (org-cliplink-uncompress-gziped-text (cdr response))
                          (cdr response)))))
    (cdr (assoc 'summary (assoc 'fields json-content)))))

;;;###autoload
(defun org-cliplink-jira-retrieve-summary (jira-base-url jira-username jira-password jira-id summary-callback)
  (let ((url (concat jira-base-url
                     "/rest/api/2/issue/"
                     jira-id
                     "?fields=summary")))
    (let ((url-request-method "GET")
          (url-request-extra-headers
           `(("Content-Type" . "application/json")
             ("Authorization" . ,(concat
                                  "Basic "
                                  (base64-encode-string
                                   (concat jira-username ":" jira-password))))))
          (dest-buffer (current-buffer)))
      (url-retrieve
       url
       `(lambda (status)
          (let ((summary (org-cliplink-jira-extract-summary-from-current-buffer)))
            (with-current-buffer ,dest-buffer
              (funcall (quote ,summary-callback)
                       (concat ,jira-base-url "/browse/" ,jira-id)
                       (org-cliplink-prepare-cliplink-title
                        (format "(%s) %s" ,jira-id summary))))))))))

;;;###autoload
(defun org-cliplink-jira ()
  (interactive)
  (let* ((jira-id (substring-no-properties (current-kill 0)))
         (jira-secrets (plist-get (org-cliplink-read-secrets) :jira))
         (jira-base-url (plist-get jira-secrets :base-url))
         (jira-username (plist-get jira-secrets :username))
         (jira-password (plist-get jira-secrets :password)))
    (org-cliplink-jira-retrieve-summary
     jira-base-url
     jira-username
     jira-password
     jira-id
     'org-cliplink-insert-org-mode-link-callback)))

;;; org-cliplink-jira.el ends here
