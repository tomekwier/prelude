;;; personal.el --- Emacs global configuration

;; Copy PATH
(setq exec-path-from-shell-variables '("PATH" "MANPATH" "GOPATH" "GOROOT"))
(exec-path-from-shell-initialize)

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
;; Turn on paren match highlighting
(show-paren-mode 1)
;; Tramp configuration
(setq tramp-default-method "ssh")
(setq tramp-terminal-type "dumb")
;; Yasnippet
(yas-global-mode 1)
;; Personal snippets file
(add-to-list 'yas/root-directory (concat prelude-personal-dir "/my-snippets"))
;; Fix ansi-term and yasnippet
(add-hook 'term-mode-hook (lambda()
                            (setq yas-dont-activate t)))

;; org-mode
;; See this post for more information:
;; https://zzamboni.org/post/beautifying-org-mode-in-emacs/
(setq org-hide-emphasis-markers t)
(font-lock-add-keywords 'org-mode
  '(("^ *\\([-]\\) "
      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(let* ((variable-tuple
        (cond ((x-list-fonts "Pragmata Pro Liga")         '(:font "Pragmata Pro Liga"))
              ((x-list-fonts "Pragmata Pro Mono Liga") '(:font "Pragmata Pro Mono Liga"))
              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
              ((x-list-fonts "Verdana")         '(:font "Verdana"))
              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
        (headline           `(:inherit default :weight regular :foreground ,base-font-color)))

  (custom-theme-set-faces
   'user
   `(org-level-8 ((t (,@headline ,@variable-tuple))))
   `(org-level-7 ((t (,@headline ,@variable-tuple))))
   `(org-level-6 ((t (,@headline ,@variable-tuple))))
   `(org-level-5 ((t (,@headline ,@variable-tuple))))
   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.0))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.0))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.0))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.2 :weight regular :underline nil))))
   `(org-document-title ((t (,@headline ,@variable-tuple :height 1.4 :underline t))))))

(custom-theme-set-faces
  'user
  '(variable-pitch ((t (:family "Pragmata Pro Liga" :height 100 :weight regular))))
  '(fixed-pitch ((t ( :family "Pragmata Pro Mono Liga" :height 120 :weight regular)))))

(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

(custom-theme-set-faces
  'user
  '(org-block ((t (:inherit variable-pitch))))
  '(org-code ((t (:inherit (shadow fixed-pitch)))))
  '(org-document-info ((t (:foreground "dark orange"))))
  '(org-document-info-keyword ((t (:inherit (shadow variable-pitch)))))
  '(org-indent ((t (:inherit (org-hide variable-pitch)))))
  '(org-link ((t (:foreground "royal blue" :underline t))))
  '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  '(org-property-value ((t (:inherit fixed-pitch))) t)
  '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
  '(org-todo ((t (:inherit fixed-pitch))))
  '(org-done ((t (:inherit fixed-pitch))))
  '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.9))))
  '(org-verbatim ((t (:inherit (shadow variable-pitch))))))

;; org-roam
(setq org-roam-directory "~/gdrive/notes/org-roam")
(add-hook 'after-init-hook 'org-roam-mode)

;; org-journal
(setq org-journal-dir "~/gdrive/notes/org-journal")

;; Enable company mode
(require 'company)
(require 'company-go)
(autoload 'company-mode "company" nil t)
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-go)
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "s-l")

(use-package lsp-mode
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (typescript-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; keyboard
(global-set-key (kbd "C-c m") 'helm-mini)
(global-set-key (kbd "<f5>") 'helm-gtags-find-tag)
(global-set-key (kbd "<f6>") 'helm-gtags-find-rtag)
(global-set-key (kbd "<f7>") 'helm-gtags-find-symbol)
(global-set-key (kbd "<f8>") 'helm-gtags-find-files)
(global-set-key (kbd "<C-f11>") 'helm-do-ag)
(global-set-key (kbd "<C-f12>") 'helm-projectile-find-file)
