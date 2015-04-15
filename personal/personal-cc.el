;;; personal-cc.el --- Personal configuration for C & C++ development

(defun configure-irony ()
  ;; irony-mode configuration
  (company-mode)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  )

(configure-irony)

(defun my-c-cpp-hook ()
  "A common C/C++ hook for code indentation"
  ;; Do not indent in extern
  (c-set-offset 'inextern-lang 0)
  ;; Do not indent enumerations
  (c-set-offset 'brace-list-open 0)
  ;; Do not indent under namespaces
  (c-set-offset 'innamespace 0)
  ;; Other indent settings
  (c-set-offset 'inline-open 0)
  (c-set-offset 'substatement-open 0)
  (setq indent-tabs-mode nil)
  (setq c-default-style "linux")
  (if (system-is-my-homepc)
      (setq c-basic-offset 2)
    (setq c-basic-offset 4))
  (highlight-symbol-mode)
  (modify-syntax-entry ?_ "w" c++-mode-syntax-table)
  (setq highlight-symbol-idle-delay 1.5))

(add-hook 'c++-mode-hook 'my-c-cpp-hook)
(add-hook 'c-mode-hook 'my-c-cpp-hook)

;; Treat headers as c++ headers
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;; personal-cc.el ends here
