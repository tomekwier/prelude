;;; personal.el --- Emacs global configuration

(defun system-is-my-homepc ()
  "Return true if the system I am running on is my PC at home."
  (interactive)
  (string-equal system-name "curry"))

; activate all the packages (in particular autoloads)
(require 'package)

(setq package-list '(
		     exec-path-from-shell
		     go-mode
                     go-errcheck
                     go-guru
                     go-gopath
                     go-projectile
                     csharp-mode
                     js2-mode
                     js2-refactor
                     ensime
                     irony
                     ggtags
                     tern
                     company
                     company-ghc
                     company-go
                     company-irony
                     company-tern
                     company-jedi
                     elpy
                     jedi
                     jedi-core
                     highlight-symbol
                     helm-gtags		     
                     helm-projectile
                     helm-ag
                     solarized-theme
                     exec-path-from-shell
                     web-beautify
                     dimmer
                     yasnippet))

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; enable dimmer mode
(dimmer-activate)

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Copy PATH
(setq exec-path-from-shell-variables '("PATH" "MANPATH" "GOPATH" "GOROOT"))
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
(load-theme 'deeper-blue t)
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
(require 'company-go)
(autoload 'company-mode "company" nil t)
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-tern)
(add-to-list 'company-backends 'company-go)
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing

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
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-hook 'js2-mode-hook (lambda ()
                           (progn
                             (require 'js2-refactor)
                             (company-mode)
                             (tern-mode)
                             (setq js2-basic-offset 2)
                             )))

(setq js2-highlight-level 3)

(add-hook 'typescript-mode-hook (lambda ()
                                  (progn
                                    (setq js2-basic-offset 2)
                                    (setq typescript-indent-level 2)
                                    )))


(add-hook 'powershell-mode-hook (lambda ()
                                  (progn
                                    (setq powershell-indent 2)
                                    )))

;; company-tern
(add-to-list 'company-backends 'company-tern)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; web-mode configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; json indent
(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq json-reformat:indent-width 2)
            (setq js-indent-level 2)))

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
;; Enable elpy
(elpy-enable)
;; (setq elpy-rpc-python-command "python3")

(defun python-custom-hook ()
  (autoload 'jedi:setup "jedi" nil t)
  (setq indent-tabs-mode nil
        python-indent 4)
  (jedi:setup)
  (jedi:ac-setup)
  (add-to-list 'company-backends 'company-jedi)
  (setq jedi:setup-keys t)
  (define-key elpy-mode-map (kbd "M-.") 'jedi:goto-definition)
  )

;; Python mode hooks
(add-hook 'python-mode-hook 'python-custom-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Scala configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Golang configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Setup go-guru
(require 'go-guru)

(defun my-go-mode-hook ()
; Call Gofmt before saving                                                    
  (add-hook 'before-save-hook 'gofmt-before-save)
; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go generate && go build -v && go test -v && go vet"))
; Godef jump key binding                                                      
  (local-set-key (kbd "M-.") 'go-guru-definition)
; Go-guru hl-identifier mode
  (go-guru-hl-identifier-mode)
  )

(add-hook 'go-mode-hook 'my-go-mode-hook)

(defun my-json-mode-hook ()
  (setq-default js2-basic-offset 2
                js-indent-level 2
                json-reformat:indent-width 2))
(add-hook 'json-mode-hook 'my-json-mode-hook)

(defun json-to-single-line (beg end)
  "Collapse prettified json in region between BEG and END to a single line"
  (interactive "r")
  (if (use-region-p)
      (save-excursion
        (save-restriction
          (narrow-to-region beg end)
          (goto-char (point-min))
          (while (re-search-forward "\\s-+" nil t)
            (replace-match " "))))
    (print "This function operates on a region")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Keyboard configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm & gtags
(global-set-key (kbd "C-c m") 'helm-mini)
(global-set-key (kbd "<f5>") 'helm-gtags-find-tag)
(global-set-key (kbd "<f6>") 'helm-gtags-find-rtag)
(global-set-key (kbd "<f7>") 'helm-gtags-find-symbol)
(global-set-key (kbd "<f8>") 'helm-gtags-find-files)
(global-set-key (kbd "<C-f11>") 'helm-do-ag)
(global-set-key (kbd "<C-f12>") 'helm-projectile-find-file)

;; Compilation
(global-set-key (kbd "<C-f4>") '(lambda () (interactive) (compile "make clean")))
(global-set-key (kbd "<C-f5>") '(lambda () (interactive) (compile "make all")))
(global-set-key (kbd "<C-f6>") '(lambda () (interactive) (compile "make test")))

;; highlight-symbol
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

; https://raw.githubusercontent.com/fabrizioschiavi/pragmatapro/master/emacs_snippets/pragmatapro-prettify-symbols-v0.827.el
(setq prettify-symbols-unprettify-at-point 'right-edge)

(defconst pragmatapro-prettify-symbols-alist
  (mapcar (lambda (s)
            `(,(car s)
              .
              ,(vconcat
                (apply 'vconcat
                       (make-list
                        (- (length (car s)) 1)
                        (vector (decode-char 'ucs #X0020) '(Br . Bl))))
                (vector (decode-char 'ucs (cadr s))))))
          '(("[ERROR]"   #XE380)
            ("[DEBUG]"   #XE381)
            ("[INFO]"    #XE382)
            ("[WARN]"    #XE383)
            ("[WARNING]" #XE384)
            ("[ERR]"     #XE385)
            ("[FATAL]"   #XE386)
            ("[TRACE]"   #XE387)
            ("[FIXME]"   #XE388)
            ("[TODO]"    #XE389)
            ("[BUG]"     #XE38A)
            ("[NOTE]"    #XE38B)
            ("[HACK]"    #XE38C)
            ("[MARK]"    #XE38D)
            ("!!"        #XE900)
            ("!="        #XE901)
            ("!=="       #XE902)
            ("!!!"       #XE903)
            ("!≡"        #XE904)
            ("!≡≡"       #XE905)
            ("!>"        #XE906)
            ("!=<"       #XE907)
            ("#("        #XE920)
            ("#_"        #XE921)
            ("#{"        #XE922)
            ("#?"        #XE923)
            ("#>"        #XE924)
            ("##"        #XE925)
            ("#_("       #XE926)
            ("%="        #XE930)
            ("%>"        #XE931)
            ("%>%"       #XE932)
            ("%<%"       #XE933)
            ("&%"        #XE940)
            ("&&"        #XE941)
            ("&*"        #XE942)
            ("&+"        #XE943)
            ("&-"        #XE944)
            ("&/"        #XE945)
            ("&="        #XE946)
            ("&&&"       #XE947)
            ("&>"        #XE948)
            ("$>"        #XE955)
            ("***"       #XE960)
            ("*="        #XE961)
            ("*/"        #XE962)
            ("*>"        #XE963)
            ("++"        #XE970)
            ("+++"       #XE971)
            ("+="        #XE972)
            ("+>"        #XE973)
            ("++="       #XE974)
            ("--"        #XE980)
            ("-<"        #XE981)
            ("-<<"       #XE982)
            ("-="        #XE983)
            ("->"        #XE984)
            ("->>"       #XE985)
            ("---"       #XE986)
            ("-->"       #XE987)
            ("-+-"       #XE988)
            ("-\\/"      #XE989)
            ("-|>"       #XE98A)
            ("-<|"       #XE98B)
            (".."        #XE990)
            ("..."       #XE991)
            ("..<"       #XE992)
            (".>"        #XE993)
            (".~"        #XE994)
            (".="        #XE995)
            ("/*"        #XE9A0)
            ("//"        #XE9A1)
            ("/>"        #XE9A2)
            ("/="        #XE9A3)
            ("/=="       #XE9A4)
            ("///"       #XE9A5)
            ("/**"       #XE9A6)
            (":::"       #XE9AF)
            ("::"        #XE9B0)
            (":="        #XE9B1)
            (":≡"        #XE9B2)
            (":>"        #XE9B3)
            (":=>"       #XE9B4)
            (":("        #XE9B5)
            (":-("       #XE9B6)
            (":)"        #XE9B7)
            (":-)"       #XE9B8)
            (":/"        #XE9B9)
            (":\\"       #XE9BA)
            (":3"        #XE9BB)
            (":D"        #XE9BC)
            (":P"        #XE9BD)
            (":>:"       #XE9BE)
            (":<:"       #XE9BF)
            ("<$>"       #XE9C0)
            ("<*"        #XE9C1)
            ("<*>"       #XE9C2)
            ("<+>"       #XE9C3)
            ("<-"        #XE9C4)
            ("<<"        #XE9C5)
            ("<<<"       #XE9C6)
            ("<<="       #XE9C7)
            ("<="        #XE9C8)
            ("<=>"       #XE9C9)
            ("<>"        #XE9CA)
            ("<|>"       #XE9CB)
            ("<<-"       #XE9CC)
            ("<|"        #XE9CD)
            ("<=<"       #XE9CE)
            ("<~"        #XE9CF)
            ("<~~"       #XE9D0)
            ("<<~"       #XE9D1)
            ("<$"        #XE9D2)
            ("<+"        #XE9D3)
            ("<!>"       #XE9D4)
            ("<@>"       #XE9D5)
            ("<#>"       #XE9D6)
            ("<%>"       #XE9D7)
            ("<^>"       #XE9D8)
            ("<&>"       #XE9D9)
            ("<?>"       #XE9DA)
            ("<.>"       #XE9DB)
            ("</>"       #XE9DC)
            ("<\\>"      #XE9DD)
            ("<\">"      #XE9DE)
            ("<:>"       #XE9DF)
            ("<~>"       #XE9E0)
            ("<**>"      #XE9E1)
            ("<<^"       #XE9E2)
            ("<!"        #XE9E3)
            ("<@"        #XE9E4)
            ("<#"        #XE9E5)
            ("<%"        #XE9E6)
            ("<^"        #XE9E7)
            ("<&"        #XE9E8)
            ("<?"        #XE9E9)
            ("<."        #XE9EA)
            ("</"        #XE9EB)
            ("<\\"       #XE9EC)
            ("<\""       #XE9ED)
            ("<:"        #XE9EE)
            ("<->"       #XE9EF)
            ("<!--"      #XE9F0)
            ("<--"       #XE9F1)
            ("<~<"       #XE9F2)
            ("<==>"      #XE9F3)
            ("<|-"       #XE9F4)
            ("<<|"       #XE9F5)
            ("<-<"       #XE9F7)
            ("<-->"      #XE9F8)
            ("<<=="      #XE9F9)
            ("<=="       #XE9FA)
            ("==<"       #XEA00)
            ("=="        #XEA01)
            ("==="       #XEA02)
            ("==>"       #XEA03)
            ("=>"        #XEA04)
            ("=~"        #XEA05)
            ("=>>"       #XEA06)
            ("=/="       #XEA07)
            ("=~="       #XEA08)
            ("==>>"      #XEA09)
            ("≡≡"        #XEA10)
            ("≡≡≡"       #XEA11)
            ("≡:≡"       #XEA12)
            (">-"        #XEA20)
            (">="        #XEA21)
            (">>"        #XEA22)
            (">>-"       #XEA23)
            (">=="       #XEA24)
            (">>>"       #XEA25)
            (">=>"       #XEA26)
            (">>^"       #XEA27)
            (">>|"       #XEA28)
            (">!="       #XEA29)
            (">->"       #XEA2A)
            ("??"        #XEA40)
            ("?~"        #XEA41)
            ("?="        #XEA42)
            ("?>"        #XEA43)
            ("???"       #XEA44)
            ("?."        #XEA45)
            ("^="        #XEA48)
            ("^."        #XEA49)
            ("^?"        #XEA4A)
            ("^.."       #XEA4B)
            ("^<<"       #XEA4C)
            ("^>>"       #XEA4D)
            ("^>"        #XEA4E)
            ("\\\\"      #XEA50)
            ("\\>"       #XEA51)
            ("\\/-"      #XEA52)
            ("@>"        #XEA57)
            ("|="        #XEA60)
            ("||"        #XEA61)
            ("|>"        #XEA62)
            ("|||"       #XEA63)
            ("|+|"       #XEA64)
            ("|->"       #XEA65)
            ("|-->"      #XEA66)
            ("|=>"       #XEA67)
            ("|==>"      #XEA68)
            ("|>-"       #XEA69)
            ("|<<"       #XEA6A)
            ("||>"       #XEA6B)
            ("|>>"       #XEA6C)
            ("|-"        #XEA6D)
            ("||-"       #XEA6E)
            ("~="        #XEA70)
            ("~>"        #XEA71)
            ("~~>"       #XEA72)
            ("~>>"       #XEA73)
            ("[["        #XEA80)
            ("]]"        #XEA81)
            ("\">"       #XEA90)
            ("_|_"       #XEA97)
            )))

(defun add-pragmatapro-prettify-symbols-alist ()
  (dolist (alias pragmatapro-prettify-symbols-alist)
    (push alias prettify-symbols-alist)))

  (defun refresh-pretty ()
    (prettify-symbols-mode -1)
    (prettify-symbols-mode +1))

  ;; Hooks for modes in which to install the Pragmata ligatures
  (mapc (lambda (hook)
          (add-hook hook (lambda () (add-pragmatapro-prettify-symbols-alist) (refresh-pretty))))
        '(text-mode-hook
          prog-mode-hook))
  (global-prettify-symbols-mode +1)

(provide 'personal-config)
;;; personal.el ends here
