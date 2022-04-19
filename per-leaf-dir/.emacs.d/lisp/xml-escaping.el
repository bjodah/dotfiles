;;; package --- Summary
;;; Commentary:
;;; Code:
(defun de-escape (beg end)
  "De-escape the region.  From BEG to END."
  (interactive "*r")
  (save-restriction
    (narrow-to-region beg end)
    (let ((replacements '(("&lt;" "<")
                          ("&gt;" ">")
                          ("&amp;" "&")
                          ("&quot;" "\"")
                          ("&#xa;" "\n")))
          (case-fold-search nil))
      (dolist (replacement replacements)
        (cl-destructuring-bind (old new) replacement
          (goto-char (point-min))
          (while (search-forward old nil t)
            (replace-match new))))
    )))

(provide 'de-escape)

;;; xml-escaping.el ends here
