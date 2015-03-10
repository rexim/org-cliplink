(require 'ert)

(add-to-list 'load-path ".")
(load "org-cliplink.el")

(ert-deftest org-cliplink-parse-response-test ()
  (let ((test-data '(("test-data/responses/inconsistent-eol-response" .
                      ((("Last-Modified" . "Sun, 08 Mar 2015 14:06:08 GMT")
                        ("Content-Length" . "199")
                        ("Content-type" . "text/html")
                        ("Date" . "Sun, 08 Mar 2015 14:17:14 GMT")
                        ("Server" . "SimpleHTTP/0.6 Python/2.7.9")) .
                        "\nHere goes body\n"))
                     ("test-data/responses/correct-response-without-title" .
                      ((("Last-Modified" . "Sun, 08 Mar 2015 14:06:08 GMT")
                        ("Content-Length" . "199")
                        ("Content-type" . "text/html")
                        ("Date" . "Sun, 08 Mar 2015 14:17:14 GMT")
                        ("Server" . "SimpleHTTP/0.6 Python/2.7.9")) .
                        "\nHere goes body\n")))))
    (dolist (test-case test-data)
      (message (car test-case))
      (let ((data-file (car test-case))
            (expected-outcome (cdr test-case)))
        (with-temp-buffer
          (insert-file data-file)
          (should (equal (org-cliplink-parse-response) expected-outcome)))))))

(ert-run-tests-batch-and-exit)
