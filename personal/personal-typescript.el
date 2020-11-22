(add-hook 'typescript-mode-hook (lambda ()
                                  (progn
                                    (setq js2-basic-offset 2)
                                    (setq typescript-indent-level 2)
                                    )))
