;;; org-cliplink-transport.el --- insert org-mode links from the clipboard -*- lexical-binding: t -*-

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

(require 'url-parse)

(require 'org-cliplink-string)

(defun org-cliplink-curl-prepare-response-buffer-name (url)
  (format " *curl-%s-%x*"
          (url-host (url-generic-parse-url url))
          (random)))

(defun org-cliplink-http-get-request--curl (url callback &optional basic-auth-credentials)
  (let* ((response-buffer-name (org-cliplink-curl-prepare-response-buffer-name url))
         (curl-arguments
          (append (list "--include"
                        "--silent"
                        "--show-error"
                        "-X"
                        "GET")
                  (when basic-auth-credentials
                    (let ((username (plist-get basic-auth-credentials :username))
                          (password (plist-get basic-auth-credentials :password)))
                      (message (format "Curl Basic Auth: %s %s" username password))
                      (list "--user"
                            (format "%s:%s" username password))))
                  (list url)))
         (curl-process (apply #'start-process
                              "curl"
                              response-buffer-name
                              (executable-find "curl")
                              curl-arguments)))
    (set-process-sentinel curl-process
                          (lambda (process event)
                            (when (not (process-live-p process))
                              (if (zerop (process-exit-status process))
                                  (when callback
                                    (with-current-buffer response-buffer-name
                                      (funcall callback nil)))
                                (with-current-buffer response-buffer-name
                                  (error (buffer-string)))))))))

(defun org-cliplink-http-get-request (url callback &optional basic-auth-credentials)
  (org-cliplink-http-get-request--curl url callback basic-auth-credentials))

(provide 'org-cliplink-transport)

;;; org-cliplink-transport.el ends here
