;;; personal.el

(require 'yasnippet)
(require 'helm-config)
(require 'helm-gtags)
(require 'highlight-symbol)
(require 'use-package)

;; Displays the time in the status bar
(display-time)
;; Paren mode
(show-paren-mode t)
;; Default theme
(load-theme 'zenburn t)
;; Blink
(blink-cursor-mode t)
;; No guru mode
(setq prelude-guru nil)
;; Display menu
(menu-bar-mode)
;; Ediff -  Side by side is the only way
(setq ediff-split-window-function 'split-window-horizontally)
;; Ack to always ask about directory
(setq ack-and-a-half-prompt-for-directory t)

;; Yasnippet
(yas-global-mode 1)
;; Personal snippets file
(add-to-list 'yas/root-directory (concat prelude-personal-dir "/my-snippets"))

;; keyboard
(global-set-key (kbd "C-c m") 'helm-mini)
(global-set-key (kbd "<f5>") 'helm-gtags-find-tag)
(global-set-key (kbd "<f6>") 'helm-gtags-find-rtag)
(global-set-key (kbd "<f7>") 'helm-gtags-find-symbol)
(global-set-key (kbd "<f8>") 'helm-gtags-find-files)
(global-set-key (kbd "<C-f11>") 'helm-do-ag)
(global-set-key (kbd "<C-f12>") 'helm-projectile-find-file)
