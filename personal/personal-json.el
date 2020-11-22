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
