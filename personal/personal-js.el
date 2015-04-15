;;; personal-js.el --- Javascript configuration

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;;(add-hook 'js-mode-hook 'js2-minor-mode)
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-hook 'js2-mode-hook (lambda ()
                           (progn
                             (require 'js2-refactor)
                             (company-mode)
                             (tern-mode)
                             (company-tern))))

(setq js2-highlight-level 3)

;; company-tern
(add-to-list 'company-backends 'company-tern)

(provide 'personal-js)
