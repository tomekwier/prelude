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
