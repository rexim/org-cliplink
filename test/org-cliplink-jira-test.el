(require 'el-mock)

(add-to-list 'load-path ".")
(load "org-cliplink-jira.el")

(ert-deftest org-cliplink-jira-extract-summary-from-current-buffer-test ()
  (with-mock
   (stub org-cliplink-parse-response =>
         '(nil . "{\"fields\":{\"summary\":\"Hello, World\"}}"))
   (should (equal (org-cliplink-jira-extract-summary-from-current-buffer)
                  "Hello, World")))

  (with-mock
   (stub org-cliplink-parse-response =>
         '(nil . "{}"))
   (should (not (org-cliplink-jira-extract-summary-from-current-buffer)))))

(ert-deftest org-cliplink-jira-extract-jira-id-from-url-test ()
  (should (equal (org-cliplink-jira-extract-jira-id-from-url
                  "http://rexim.me/jira/"
                  "http://rexim.me/jira/browse/REXIM-6222")
                 "REXIM-6222"))
  (should (not (org-cliplink-jira-extract-jira-id-from-url
                "http://fornever.me/jira/"
                "http://rexim.me/jira/browse/REXIM-6222"))))
