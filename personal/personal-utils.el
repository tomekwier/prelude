;;; personal-utils.el --- Helper functions

(defun system-is-my-homepc ()
  "Return true if the system I am running on is my PC at home."
  (interactive)
  (string-equal system-name "hs"))

(provide 'personal-utils)
;;; personal-utils.el ends here
