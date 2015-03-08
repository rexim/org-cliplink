(add-to-list 'load-path ".")
(load "org-cliplink.el")

(let ((url "http://127.0.0.1:3001/")
      (expected-outcome "[[http://127.0.0.1:3001/][&{Hello} '{World} α  ]]")
      (timeout 5))
  (with-temp-buffer
    (kill-new url)
    (org-cliplink)
    (sleep-for timeout)
    (let ((actual-outcome (buffer-string)))
      (if (string= expected-outcome (buffer-string))
          (message "Passed")
        (message "Failed: expected: \"%s\", actual: \"%s\""
                 expected-outcome
                 actual-outcome)
        (kill-emacs 1)))))
