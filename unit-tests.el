(add-to-list 'load-path ".")
(load "org-cliplink.el")

(defun test-org-cliplink-parse-response (response-filename expected-outcome)
  (with-temp-buffer
    (insert-file response-filename)
    (condition-case err
        (let ((actual-outcome (org-cliplink-parse-response)))
          (unless (equal actual-outcome expected-outcome)
            (message "org-cliplink-parse-response: %s" response-filename)
            (message "expected outcome is")
            (pp expected-outcome)
            (message "but actual outcome is")
            (pp actual-outcome)
            (kill-emacs 1))) 
      (error (progn (message "org-cliplink-parse-response signaled an error on %s: %s"
                             response-filename err)
                    (kill-emacs 1))))
    (message "org-cliplink-parse-response: %s passed" response-filename)))

(test-org-cliplink-parse-response "test-data/responses/inconsistent-eol-response"
                                  '((("Last-Modified" . "Sun, 08 Mar 2015 14:06:08 GMT")
                                     ("Content-Length" . "199")
                                     ("Content-type" . "text/html")
                                     ("Date" . "Sun, 08 Mar 2015 14:17:14 GMT")
                                     ("Server" . "SimpleHTTP/0.6 Python/2.7.9")) .
                                     "\nHere goes body\n"))
(test-org-cliplink-parse-response "test-data/responses/correct-response-without-title"
                                  '((("Last-Modified" . "Sun, 08 Mar 2015 14:06:08 GMT")
                                     ("Content-Length" . "199")
                                     ("Content-type" . "text/html")
                                     ("Date" . "Sun, 08 Mar 2015 14:17:14 GMT")
                                     ("Server" . "SimpleHTTP/0.6 Python/2.7.9")) .
                                     "\nHere goes body\n"))
