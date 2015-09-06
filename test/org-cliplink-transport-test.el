(ert-deftest org-cliplink-curl-prepare-response-buffer-name ()
  (let ((url "http://rexim.me/"))
    (should (string-match " \\*curl-rexim.me-[a-z0-9]+"
                          (org-cliplink-curl-prepare-response-buffer-name url)))))

(ert-deftest org-cliplink-credentials-to-basic-auth-test ()
  (should (equal "Basic aGVsbG86d29ybGQ="
                 (org-cliplink-credentials-to-basic-auth "hello" "world"))))

(ert-deftest org-cliplink-build-curl-arguments-test ()
  (let* ((url "http://rexim.me")
         (username "rexim")
         (password "nyasha")
         (extra-arguments (list "--secure"))
         (expected-output (list "--secure"
                                "--include"
                                "--silent"
                                "--show-error"
                                "-X" "GET"
                                "--user"
                                (concat username ":" password)
                                url)))
    (should (equal expected-output
                   (org-cliplink-build-curl-arguments url
                                                      (list :username username
                                                            :password password)
                                                      extra-arguments)))))
