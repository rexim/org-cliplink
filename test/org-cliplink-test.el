(require 'el-mock)

(add-to-list 'load-path ".")
(load "org-cliplink.el")

(ert-deftest org-cliplink-parse-response-test ()
  (let ((test-data '(("test-data/responses/inconsistent-eol-response" .
                      ((("Last-Modified" . "Sun, 08 Mar 2015 14:06:08 GMT")
                        ("Content-Length" . "199")
                        ("Content-type" . "text/html")
                        ("Date" . "Sun, 08 Mar 2015 14:17:14 GMT")
                        ("Server" . "SimpleHTTP/0.6 Python/2.7.9")) .
                        "Here goes body\n"))
                     ("test-data/responses/correct-response-without-title" .
                      ((("Last-Modified" . "Sun, 08 Mar 2015 14:06:08 GMT")
                        ("Content-Length" . "199")
                        ("Content-type" . "text/html")
                        ("Date" . "Sun, 08 Mar 2015 14:17:14 GMT")
                        ("Server" . "SimpleHTTP/0.6 Python/2.7.9")) .
                        "Here goes body\n")))))
    (dolist (test-case test-data)
      (message (car test-case))
      (let ((data-file (car test-case))
            (expected-outcome (cdr test-case)))
        (with-temp-buffer
          (insert-file data-file)
          (should (equal (org-cliplink-parse-response) expected-outcome)))))))


(ert-deftest org-cliplink-read-secrets-positive-test ()
  (let ((org-cliplink-secrets-path "./test-data/secrets/org-cliplink-secrets.el"))
    (should (equal (org-cliplink-read-secrets)
                   '(:hello (1 2 3)))))
  (let ((org-cliplink-secrets-path "/path/to/non/existing/secrets"))
    (should (not (org-cliplink-read-secrets)))))

(ert-deftest org-cliplink-straight-string-test ()
  (should (equal (org-cliplink-straight-string "   hello    world   ")
                 "hello world")))

(ert-deftest org-cliplink-remove-string-prefix-test ()
  (should (equal (org-cliplink-remove-string-prefix "hello world" "hello")
                 " world"))
  (should (equal (org-cliplink-remove-string-prefix "hello" "hello")
                 ""))
  (should (not (org-cliplink-remove-string-prefix "foo" "foobar")))
  (should (not (org-cliplink-remove-string-prefix "hello world" "world"))))

(ert-deftest org-cliplink-check-basic-auth-for-url-test ()
  (let ((org-cliplink-secrets-path "./test-data/secrets/org-cliplink-basic-auth-secrets.el"))
    (should (equal (org-cliplink-check-basic-auth-for-url "http://rexim.me/test")
                   '(:url-pattern "http://rexim.me/*" :username "horta" :password "hell")))
    (should (not (org-cliplink-check-basic-auth-for-url "http://fornever.me/test")))))
