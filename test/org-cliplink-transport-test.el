(ert-deftest org-cliplink-curl-prepare-response-buffer-name ()
  (let ((url "http://rexim.me/"))
    (should (string-match " \\*curl-rexim.me-[a-z0-9]+"
                          (org-cliplink-curl-prepare-response-buffer-name url)))))
