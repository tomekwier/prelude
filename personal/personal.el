;;; personal.el --- Emacs global configuration

(defun system-is-my-homepc ()
  "Return true if the system I am running on is my PC at home."
  (interactive)
  (string-equal system-name "curry"))

; activate all the packages (in particular autoloads)
(require 'package)
(package-initialize)

(setq package-list '(
		     exec-path-from-shell
		     go-mode
                     csharp-mode
                     js2-mode
                     scala-mode2
                     ensime
                     irony
                     ggtags
                     tern
                     company
                     company-ghc
                     company-go
                     company-irony
                     company-tern
                     highlight-symbol
                     helm-gtags		     
                     helm-projectile
                     solarized-theme
                     exec-path-from-shell
                     web-beautify
                     yasnippet))

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Copy PATH
(exec-path-from-shell-initialize)

(require 'yasnippet)
(require 'helm-config)
(require 'helm-gtags)
(require 'highlight-symbol)

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

;; Enable company mode
(require 'company)
(autoload 'company-mode "company" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Haskell configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))

(custom-set-variables '(haskell-tags-on-save t))

(custom-set-variables
  '(haskell-process-suggest-remove-import-lines t)
  '(haskell-process-auto-import-loaded-modules t)
  '(haskell-process-log t))
(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))
(eval-after-load 'haskell-cabal '(progn
  (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))
(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile))
(eval-after-load 'haskell-cabal
  '(define-key haskell-cabal-mode-map (kbd "C-c C-o") 'haskell-compile))

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

(add-hook 'haskell-mode-hook 'company-mode)
(add-to-list 'company-backends 'company-ghc)
(custom-set-variables '(company-ghc-show-info t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Javascript configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; C/C++ configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Python configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun python-custom-hook ()
;;   (autoload 'jedi:setup "jedi" nil t)
;;   (setq indent-tabs-mode nil
;;         python-indent 2)
;; )

;; ;; Python mode hooks
;; (add-hook 'python-mode-hook 'python-custom-hook)
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (add-hook 'python-mode-hook 'jedi:ac-setup)
;; (setq jedi:setup-keys t)

;; ;; Enable elpy
;; (elpy-enable)
;; (setq elpy-rpc-python-command "python3")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Scala configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Keyboard configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm & gtags
(global-set-key (kbd "C-c m") 'helm-mini)
(global-set-key (kbd "<f5>") 'helm-gtags-find-tag)
(global-set-key (kbd "<f6>") 'helm-gtags-find-rtag)
(global-set-key (kbd "<f7>") 'helm-gtags-find-symbol)
(global-set-key (kbd "<f8>") 'helm-gtags-find-files)

;; Compilation
(global-set-key (kbd "<C-f4>") '(lambda () (interactive) (compile "make clean")))
(global-set-key (kbd "<C-f5>") '(lambda () (interactive) (compile "make all")))
(global-set-key (kbd "<C-f6>") '(lambda () (interactive) (compile "make test")))

;; highlight-symbol
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

(provide 'personal-config)
;;; personal.el ends here
