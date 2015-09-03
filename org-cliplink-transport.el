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

(require 'org-cliplink-string)

(defun org-cliplink-http-get-request (url callback &optional basic-auth-credentials)
  (let* ((url-hash (secure-hash 'sha1 url))
         (response-buffer-name (format "%s" url-hash))
         (curl-process (start-process (concat "curl-" url-hash)
                                      response-buffer-name
                                      (executable-find "curl")
                                      "--include"
                                      "--silent"
                                      "--show-error"
                                      "-X"
                                      "GET"
                                      url)))
    (set-process-sentinel curl-process
                          (lambda (process event)
                            (when callback
                              (with-current-buffer response-buffer-name
                                (funcall callback nil)))))))

(provide 'org-cliplink-transport)

;;; org-cliplink-transport.el ends here
