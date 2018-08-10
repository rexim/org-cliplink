(require 'ert)

(add-to-list 'load-path ".")
(load "org-cliplink.el")

(custom-set-variables
 '(org-cliplink-transport-implementation (quote url-el))
 '(network-security-level (quote low)))

(ert-deftest org-cliplink-without-title--http ()
  (let ((url "http://127.0.0.1:8001/without-title.html")
        (expected-outcome "[[http://127.0.0.1:8001/without-title.html]]")
        (timeout 5))
    (with-temp-buffer
      (kill-new url)
      (org-cliplink)
      (sleep-for timeout)
      (should (equal (buffer-string) expected-outcome)))))

(ert-deftest org-cliplink-long-title-with-custom-transformer--http ()
  (let ((url "http://127.0.0.1:8001/long-title.html")
        (expected-outcome "[[http://127.0.0.1:8001/long-title.html][long title]]")
        (timeout 1)
        (custom-org-cliplink
         (lambda ()
           (org-cliplink-insert-transformed-title
            (org-cliplink-clipboard-content)
            (lambda (url title)
              (org-cliplink-org-mode-link-transformer
               url
               (replace-regexp-in-string "\\(very \\)+\\([[:alpha:][:space:]]+\\)" "\\2" title)))))))
    (with-temp-buffer
      (kill-new url)
      (funcall custom-org-cliplink)
      (sleep-for timeout)
      (should (equal (buffer-string) expected-outcome)))))

(ert-deftest org-cliplink-retrieve-long-title-synchronously--http ()
  (let ((url "http://127.0.0.1:8001/long-title.html")
        (expected-outcome "very very very very very very very very very very very very very very very ve..."))
    (should (equal (org-cliplink-retrieve-title-synchronously url)
                   expected-outcome))))

(ert-deftest org-cliplink-simple-title--http ()
  (let ((url "http://127.0.0.1:8001/http.html")
        (expected-outcome "[[http://127.0.0.1:8001/http.html][Hello World]]")
        (timeout 5))
    (with-temp-buffer
      (kill-new url)
      (org-cliplink)
      (sleep-for timeout)
      (should (equal (buffer-string) expected-outcome)))))

(ert-deftest org-cliplink-escape-title--http ()
  (let ((url "http://127.0.0.1:8001/html4-escaping.html")
        (expected-outcome "[[http://127.0.0.1:8001/html4-escaping.html][&{Hello} '{World} α  ]]")
        (timeout 5))
    (with-temp-buffer
      (kill-new url)
      (org-cliplink)
      (sleep-for timeout)
      (should (equal (buffer-string) expected-outcome)))))

(ert-deftest org-cliplink-simple-title--https ()
  (let ((url "https://127.0.0.1:4443/http.html")
        (expected-outcome "[[https://127.0.0.1:4443/http.html][Hello World]]")
        (timeout 5))
    (with-temp-buffer
      (kill-new url)
      (org-cliplink)
      (sleep-for timeout)
      (should (equal (buffer-string) expected-outcome)))))

(ert-deftest org-cliplink-simple-title--gziped-http ()
  (let ((url "http://127.0.0.1:8002/http.html")
        (expected-outcome "[[http://127.0.0.1:8002/http.html][Hello World]]")
        (timeout 5))
    (with-temp-buffer
      (kill-new url)
      (org-cliplink)
      (sleep-for timeout)
      (should (equal (buffer-string) expected-outcome)))))

(ert-deftest org-cliplink-escape-title--gziped-https ()
  (let ((url "https://127.0.0.1:4444/html4-escaping.html")
        (expected-outcome "[[https://127.0.0.1:4444/html4-escaping.html][&{Hello} '{World} α  ]]")
        (timeout 5))
    (with-temp-buffer
      (kill-new url)
      (org-cliplink)
      (sleep-for timeout)
      (should (equal (buffer-string) expected-outcome)))))

(ert-deftest org-cliplink-simple-title--http-with-basic-auth ()
  (let ((url "http://127.0.0.1:8003/http.html")
        (expected-outcome "[[http://127.0.0.1:8003/http.html][Hello World]]")
        (timeout 5)
        (org-cliplink-secrets-path "./test-data/secrets/org-cliplink-basic-auth-it.el"))
    (with-temp-buffer
      (kill-new url)
      (org-cliplink)
      (sleep-for timeout)
      (should (equal (buffer-string) expected-outcome)))))

(ert-deftest org-cliplink-simple-title--https-with-basic-auth ()
  (let ((url "https://127.0.0.1:4445/http.html")
        (expected-outcome "[[https://127.0.0.1:4445/http.html][Hello World]]")
        (timeout 5)
        (org-cliplink-secrets-path "./test-data/secrets/org-cliplink-basic-auth-it.el"))
    (with-temp-buffer
      (kill-new url)
      (org-cliplink)
      (sleep-for timeout)
      (should (equal (buffer-string) expected-outcome)))))

(ert-run-tests-batch-and-exit)
