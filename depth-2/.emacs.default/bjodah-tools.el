(defun extract-using-regex (regexp group-number &optional output-buffer-name)
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
