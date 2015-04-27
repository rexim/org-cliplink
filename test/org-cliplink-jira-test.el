(require 'el-mock)

(add-to-list 'load-path ".")
(load "org-cliplink-jira.el")

(ert-deftest org-cliplink-jira-extract-summary-from-current-buffer-positive-test ()
  (with-mock
   (stub org-cliplink-parse-response =>
         '(nil . "{\"fields\":{\"summary\":\"Hello, World\"}}"))
   (should (equal (org-cliplink-jira-extract-summary-from-current-buffer)
                  "Hello, World"))))

(ert-deftest org-cliplink-jira-extract-summary-from-current-buffer-negative-test ()
  (with-mock
   (stub org-cliplink-parse-response =>
         '(nil . "{}"))
   (should (not (org-cliplink-jira-extract-summary-from-current-buffer)))))

