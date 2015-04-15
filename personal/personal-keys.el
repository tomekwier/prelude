;;; personal-keys.el --- Personal key binding configuration

(global-set-key [f11] 'toggle-frame-fullscreen)

;; Org mode settings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; helm & gtags
(global-set-key (kbd "C-c m") 'helm-mini)
(global-set-key (kbd "<f5>") 'helm-gtags-find-tag)
(global-set-key (kbd "<f6>") 'helm-gtags-find-rtag)
(global-set-key (kbd "<f7>") 'helm-gtags-find-symbol)
(global-set-key (kbd "<f8>") 'helm-gtags-find-files)

;; clang-format
(global-set-key (kbd "<C-f12>") 'clang-format-region)
(global-set-key (kbd "<C-f11>") 'clang-format-buffer)

;; Compilation
(global-set-key (kbd "<C-f4>") '(lambda () (interactive) (compile "make clean")))
(global-set-key (kbd "<C-f5>") '(lambda () (interactive) (compile "make all")))
(global-set-key (kbd "<C-f6>") '(lambda () (interactive) (compile "make test")))

;; highlight-symbol
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

;;; personal-keys.el ends here
