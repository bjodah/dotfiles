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

(defun bjodah/install-package-from-url-if-missing (package-symbol package-url)
  "Download and install a package from a URL if it's not already installed.
This version uses `curl` for robust downloading.
PACKAGE-SYMBOL is the symbol of the package (e.g., 'evil).
PACKAGE-URL is the URL to the .tar or .el file."
  (if (package-installed-p package-symbol)
      (message "Package '%s' is already installed." package-symbol)
    ;; Check if curl is available before proceeding.
    (unless (executable-find "curl")
      (error "`curl` command not found, but is required for this function"))

    (let* ((package-file-name (file-name-nondirectory package-url))
           (local-file (expand-file-name package-file-name temporary-file-directory)))
      (message "Package '%s' not found. Installing from %s..."
               package-symbol package-url)
      (unwind-protect
          (condition-case err
              (progn
                ;; Use curl to download the file.
                ;; -L: Follow redirects (essential for GitHub)
                ;; -sS: Silent mode, but show errors
                ;; -o: Output file
                (let ((exit-code (call-process "curl" nil nil nil
                                               "-sSL"
                                               "-o" local-file
                                               package-url)))
                  ;; Check if curl exited successfully. A non-zero exit code
                  ;; indicates an error (e.g., 404, network issue).
                  (unless (zerop exit-code)
                    (error "curl failed with exit code %d for URL: %s"
                           exit-code package-url)))

                (message "Downloaded to %s" local-file)

                ;; Install the package from the local file.
                (package-install-file local-file)
                (message "Successfully installed package '%s'." package-symbol))
            ;; This block runs if an error occurs.
            (error (message "Failed to install package '%s': %s" package-symbol err)))
        ;; This cleanup form runs whether there was an error or not.
        (when (file-exists-p local-file)
          (delete-file local-file)
          (message "Cleaned up temporary file: %s" local-file))))))
