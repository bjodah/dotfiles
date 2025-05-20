(defun bjodah/extract-using-regex (regexp group-number &optional output-buffer-name)
  "Extracts a regex group from matching lines into a new buffer.

REGEXP is the regular expression to use.
GROUP-NUMBER is the number of the group to extract.
OUTPUT-BUFFER-NAME is the name of the buffer to put the results in.
Defaults to \"*extract-using-regex*\" if not provided."
  (interactive
   (list
    (read-string "Regexp: ")
    (string-to-number (read-string "Group Number: "))))
  (let* ((buffer-name (or output-buffer-name "*extract-using-regex*")) ; Use let*
         (new-buffer (generate-new-buffer buffer-name))
         (lines (split-string (buffer-string) "\n")))

    (with-current-buffer new-buffer
      (erase-buffer)
      (message "Regexp: %s" regexp)
      (dolist (line lines)
        (when (string-match regexp line)
          (message "%s" line) ;; <- This never yields any output
          (insert (match-string group-number line) "\n"))))

    (switch-to-buffer new-buffer)))

(defun bjodah/mark-to-char-before-literal ()
  "Mark region from point up to (but not incl.) next occurrence of a character.
Prompts for a character, uses literal matching (no regex)."
  (interactive)
  (let ((char (read-char "Mark to character (before, literal): ")))
    (if (equal (char-to-string char) (substring (buffer-string) (point) (+ (point) 1)))
        ;; Character is at point, so no region to mark.  Move forward one,
        ;; but only if not at the end of the buffer.
        (if (< (point) (point-max))
            (forward-char)
          (message "Already at character and at end of buffer."))
      (progn
        (push-mark (point) t t)
        (let ((found (search-forward (char-to-string char) nil t)))
          (if found
              (progn
                (goto-char found)
                (backward-char) ; Back up one character
                (message "Marked to just before '%c'" char))
            (progn
              (pop-mark)
              (message "Character '%c' not found" char))))))))

(defun bjodah/vterm-execute-region-or-current-line ()
  "Insert text of current line in vterm and execute."
  (interactive)
  (require 'vterm)
  (eval-when-compile (require 'subr-x))
  (let ((command (if (region-active-p)
                     (string-trim (buffer-substring
                                   (save-excursion (region-beginning))
                                   (save-excursion (region-end))))
                   (string-trim (buffer-substring (save-excursion
                                                    (beginning-of-line)
                                                    (point))
                                                  (save-excursion
                                                    (end-of-line)
                                                    (point)))))))
    (let ((buf (current-buffer)))
      (unless (get-buffer vterm-buffer-name)
        (vterm))
      (display-buffer vterm-buffer-name t)
      (switch-to-buffer-other-window vterm-buffer-name)
      (vterm--goto-line -1)
      (message command)
      (vterm-send-string command)
      (vterm-send-return)
      (switch-to-buffer-other-window buf)
      )))

;; newline-withoug-break-of-line ;; http://stackoverflow.com/questions/5898448
(defun bjodah/newline-without-break-of-line ()
"1. move to end of the line.
 2. insert indented newline"

  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))


;; http://stackoverflow.com/questions/8674912/how-to-collapse-whitespaces-in-a-region
(defun bjodah/just-one-space-in-region (beg end)
  "replace all whitespace in the region with single spaces"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "\\s-+" nil t)
        (replace-match " ")))))

(defun bjodah/insert-buffer-name () (interactive)
  (insert (buffer-name))
)
(defun bjodah/copy-buffer-name () (interactive)
(kill-new (buffer-name)))
; Let F3 insert current file name when in minibuffer
(define-key minibuffer-local-map [f3]
  (lambda() (interactive) (insert (buffer-file-name (nth 1 (buffer-list))))))

(defun bjodah/transpose1 () (interactive)
       "Interchange characters around (but excluding) point."
       (transpose-chars 1)
       (transpose-chars 1)
       (backward-char)
       (backward-char)
       (transpose-chars 1))
