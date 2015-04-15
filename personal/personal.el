;;; personal.el --- Emacs global configuration

(setq package-list '(
                     company
                     company-ghc
                     company-go
                     company-irony
                     highlight-symbol
                     helm-gtags
                     solarized-theme
                     irony
                     tern
                     js2-mode
                     company-tern
                     exec-path-from-shell
                     web-beautify
                     yasnippet))

; activate all the packages (in particular autoloads)
(package-initialize)

;; Copy PATH
(exec-path-from-shell-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'yasnippet)
(require 'helm-config)
(require 'helm-gtags)
(require 'highlight-symbol)

;; Displays the time in the status bar
(display-time)
;; Paren mode
(show-paren-mode t)
;; Default theme
(load-theme 'solarized-dark t)
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

(provide 'personal-config)
;;; personal.el ends here
