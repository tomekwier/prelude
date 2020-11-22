(exec-path-from-shell-copy-env "PYTHONPATH")
(exec-path-from-shell-copy-env "PATH")

(setq elpy-rpc-python-command "/usr/bin/python3")
(setq python-shell-interpreter "/usr/bin/python3")

(if (eq 'system-type "darwin")
    (defvar elpy-rpc-python-command "python3")
  (defvar elpy-rpc-python-command "python"))

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(defun python-custom-hook ()
  (setq elpy-rpc-backend "jedi")
  (setq indent-tabs-mode nil
        python-indent 4)
  )

(add-hook 'python-mode-hook 'python-custom-hook)
