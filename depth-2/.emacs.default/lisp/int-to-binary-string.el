;;; package --- Summary
;;; Commentary: https://stackoverflow.com/a/20577329
;;; Code:
(defun int-to-binary-string (i)
  "Convert an integer into it's binary representation in string format."
  (let ((res ""))
    (while (not (= i 0))
      (setq res (concat (if (= 1 (logand i 1)) "1" "0") res))
      (setq i (lsh i -1)))
    (if (string= res "")
        (setq res "0"))
    res))
(provide 'int-to-binary-string)

;;; int-to-binary-string.el ends here
