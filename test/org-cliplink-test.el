(ert-deftest org-cliplink-parse-raw-header-test ()
  (should (equal
           '(("Last-Modified" . "Sun, 08 Mar 2015 14:06:08 GMT")
             ("Content-Length" . "199")
             ("Content-type" . "text/html")
             ("Date" . "Sun, 08 Mar 2015 14:17:14 GMT")
             ("Server" . "SimpleHTTP/0.6 Python/2.7.9"))
           (org-cliplink-parse-raw-header
            (concat "HTTP/1.0 200 OK\n"
                    "Server: SimpleHTTP/0.6 Python/2.7.9\n"
                    "Date: Sun, 08 Mar 2015 14:17:14 GMT\n"
                    "Content-type: text/html\n"
                    "Content-Length: 199\n"
                    "Last-Modified: Sun, 08 Mar 2015 14:06:08 GMT\n"))))
  (should (equal
           '(("Last-Modified" . "Sun, 08 Mar 2015 14:06:08 GMT")
             ("Content-Length" . "199")
             ("Content-type" . "text/html")
             ("Date" . "Sun, 08 Mar 2015 14:17:14 GMT")
             ("Server" . "SimpleHTTP/0.6 Python/2.7.9"))
           (org-cliplink-parse-raw-header
            (concat "HTTP/1.0 200 OK\n"
                    "Server: SimpleHTTP/0.6 Python/2.7.9\r\n"
                    "Date: Sun, 08 Mar 2015 14:17:14 GMT\n"
                    "Content-type: text/html\n"
                    "Content-Length: 199\r\n"
                    "Last-Modified: Sun, 08 Mar 2015 14:06:08 GMT\n")))))

(ert-deftest org-cliplink-parse-response-test ()
  (with-mock
   (stub org-cliplink-parse-raw-header => nil)
   (let ((test-data '(("test-data/responses/inconsistent-eol-response" .
                       (nil . "Here goes body\n"))
                      ("test-data/responses/correct-response-without-title" .
                       (nil . "Here goes body\n")))))
     (dolist (test-case test-data)
       (message (car test-case))
       (let ((data-file (car test-case))
             (expected-outcome (cdr test-case)))
         (with-temp-buffer
           (insert-file data-file)
           (should (equal (org-cliplink-parse-response) expected-outcome))))))))


(ert-deftest org-cliplink-read-secrets-test ()
  (let ((org-cliplink-secrets-path "./test-data/secrets/org-cliplink-secrets.el"))
    (should (equal (org-cliplink-read-secrets)
                   '(:hello (1 2 3)))))
  (let ((org-cliplink-secrets-path "/path/to/non/existing/secrets"))
    (should (not (org-cliplink-read-secrets)))))

(ert-deftest org-cliplink-straight-string-test ()
  (should (not (org-cliplink-straight-string nil)))
  (should (equal (org-cliplink-straight-string "   hello    world   ")
                 "hello world")))

(ert-deftest org-cliplink-check-basic-auth-for-url-test ()
  (let ((org-cliplink-secrets-path "./test-data/secrets/org-cliplink-basic-auth-secrets.el"))
    (should (equal (org-cliplink-check-basic-auth-for-url "http://rexim.me/test")
                   '(:url-pattern "http://rexim.me/*" :username "horta" :password "hell")))
    (should (not (org-cliplink-check-basic-auth-for-url "http://fornever.me/test")))))

(ert-deftest org-cliplink-credentials-to-basic-auth-test ()
  (should (equal "Basic aGVsbG86d29ybGQ="
                 (org-cliplink-credentials-to-basic-auth "hello" "world"))))

(ert-deftest org-cliplink-extract-and-prepare-title-from-current-buffer-test ()
  (with-mock
   (stub org-cliplink-parse-response =>
         '(nil . "<title>hello</title>"))
   (stub org-cliplink-uncompress-gziped-text =>
         (error "org-cliplink-uncompress-gziped-text: This function should not be invoked"))
   (should (equal "hello"
                  (org-cliplink-extract-and-prepare-title-from-current-buffer)))))

(ert-deftest org-cliplink-escape-html4-test ()
  (should (equal "&{Hello} '{World} α  "
                 (org-cliplink-escape-html4
                  "&amp;[Hello] &#39;[World] &alpha; &nbsp;")))
  (should (not (org-cliplink-escape-html4 nil))))

(ert-deftest org-cliplink-elide-string-test ()
  (should (not (org-cliplink-elide-string nil)))
  (let ((org-cliplink-max-length 5))
    (should (equal "test" (org-cliplink-elide-string "test")))
    (should (equal "hello" (org-cliplink-elide-string "hello")))
    (should (equal "tr..." (org-cliplink-elide-string "trinitrotoluene"))))
  (let ((org-cliplink-max-length 3))
    (should (equal "hello" (org-cliplink-elide-string "hello")))
    (should (equal (make-string 80 ?a)
                   (org-cliplink-elide-string (make-string 80 ?a))))
    (should (equal (concat (make-string 77 ?a)
                           (make-string 3 ?.))
                   (org-cliplink-elide-string (make-string 81 ?a))))))

(ert-deftest org-cliplink-insert-org-mode-link-callback-test ()
  (with-temp-buffer
    (org-cliplink-insert-org-mode-link-callback "http://google.com/" "Google")
    (should (equal (buffer-string) "[[http://google.com/][Google]]"))))
