(add-to-list 'load-path ".")
(load "org-cliplink.el")

(with-temp-buffer
  (kill-new "http://org-cliplink.rexim.me/")
  (org-cliplink)
  (sleep-for 5)
  (if (string= "[[http://org-cliplink.rexim.me/][&{Hello} '{World} α  ]]"
               (buffer-string))
      (message "Passed")
    (message "Failed")
    (kill-emacs 1)))
