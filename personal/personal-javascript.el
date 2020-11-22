(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-hook 'js2-mode-hook (lambda ()
                           (progn
                             (setq js2-basic-offset 2)
                             )))

(setq js2-highlight-level 3)
