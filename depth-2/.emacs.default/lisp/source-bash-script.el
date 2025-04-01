(defun copy-process-environment ()
  "Create a copy of the Emacs process environment, handling both cons cells and strings."
  (mapcar (lambda (x)
            (if (consp x)
                (cons (car x) (cdr x))
              x))
          process-environment))

(defun source-bash-script (script-path)
  "Source a bash script and make its exported variables available to Emacs.
SCRIPT-PATH is the path to the bash script."
  (interactive "fSource bash script: ")
  (let* ((script-path (expand-file-name script-path))
         (shell-cmd (format "%s -c '. %s && env -0'"
                            shell-file-name
                            (shell-quote-argument script-path)))
         (env-output (condition-case err
                         (shell-command-to-string shell-cmd)
                       (error (message "Error running shell command to source %s: %S" script-path err)
                              nil)))
         (env-lines (when env-output (split-string env-output "\0" t)))
         ;; Start with a copy of the current environment.
         ;; We modify this copy and then assign it back to process-environment.
         (new-env (copy-process-environment))
         (updated nil))

    (when env-lines
      (dolist (line env-lines)
        (when (string-match "^\\([^=]+\\)=\\(.*\\)$" line)
          (let* ((var-name (match-string 1 line))
                 (var-value (match-string 2 line))
                 (var-symbol (intern var-name)))

            ;; 1. Use setenv to update the OS-level environment AND
            ;;    Emacs's internal view (process-environment).
            ;;    The third argument 't' means overwrite if it exists.
            (setenv var-name var-value t)

            ;; 2. Explicitly update our *copied* list 'new-env' to ensure
            ;;    it reflects the change in the desired (SYMBOL . "VALUE") format
            ;;    for variables processed this way. setenv *should* update
            ;;    process-environment, but managing our copy explicitly is safer
            ;;    during this modification process before the final assignment.
            (let ((existing-entry (assoc var-symbol new-env)))
              (if existing-entry
                  (setcdr existing-entry var-value)
                ;; If adding a new one, add as (SYMBOL . "VALUE")
                (setq new-env (cons (cons var-symbol var-value) new-env))))
            (setq updated t))))

      ;; If we processed any variables, assign the potentially modified
      ;; list back to the global process-environment.
      ;; While setenv modifies it too, this ensures our Symbol-based
      ;; additions/updates are correctly reflected in the main list.
      (when updated
          (setq process-environment new-env)
          (message "Sourced script %s and updated environment." script-path)
          t) ; Indicate success
      (unless updated
        (message "Sourced script %s, but no new/updated variables were detected." script-path)
        t)) ; Still indicate success as the script ran

    (unless env-lines
      (message "Failed to source script %s (command failed or produced no output)." script-path)
      nil))) ; Indicate failure
(provide 'source-bash-script)
