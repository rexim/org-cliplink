;;; -*- lexical-binding: t -*-

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

(ert-deftest org-cliplink-make-curl-sentinel-test ()
  (let* ((process 42)
         (callback-invoked nil)
         (response-buffer-name "khooy"))
    (with-mock
     (mock (process-live-p 42) => nil)
     (mock (process-exit-status 42) => 0)
     (mock (curl-sentinel-callback-mock nil) => nil :times 1)
     (let ((sentinel (org-cliplink-make-curl-sentinel
                      response-buffer-name
                      #'curl-sentinel-callback-mock)))
       (generate-new-buffer response-buffer-name)
       (funcall sentinel process nil)))))
