;;; strisper.el --- Streaming Speech-to-Text using whipser -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Björn Ingvar Dahlgren

;;; Commentary:
;;
;; Speech-to-Text interface for Emacs using a tcp server (strisper-endpoint)
;; which accepts waveform data recorded by "arecord" and returns timestamped
;; text transciprtion.
;; 
;; 
;;
;;; Code:


;;; Code:

(defgroup strisper ()
  "Streaming speech-to-text"
  :group 'external)

(defcustom strisper-endpoint "http://localhost:43007"
  "URL to whisper_streaming web server."
  :type 'string
  :group 'strisper)

(defvar strisper--stdout-buffer-name "*strisper-stdout*")
(defvar strisper--stderr-buffer-name "*strisper-stderr*")
(defvar strisper--processed-buffer-name "*strisper-processed*")
(defvar strisper--rec-proc nil)
;; Define a variable to hold the target buffer for insertion
(defvar strisper--target-buffer nil
  "Buffer where strisper-record-at-point should insert text.")

(defun strisper--parse-line (ln)
  "Parses a line from the server, returning just the text."
  (let ((parts (split-string ln "[[:space:]]+" t))) ; Use regex for robustness
    (if (> (length parts) 2)
        (string-join (nthcdr 2 parts) " ")
      "")))

;; Let the process filter handle line processing directly, removing the need
;; for strisper--process-buffer-line as previously defined.

(defun strisper--process-arecord (&optional post-process-hook)
  "Start a recording process in the background.
Optionally call POST-PROCESS-HOOK function after processing output."
  (let ((buffer (get-buffer-create strisper--stdout-buffer-name)))
    ;; Create the process, setting strisper--rec-proc directly.
    ;; The filter is set using :filter.
    (setq strisper--rec-proc
          (make-process
           :name "strisper--arecord"
           :command '("sh" "-c" "arecord -f S16_LE -c1 -r 16000 -t raw -D default | nc localhost 43007")
           :buffer buffer
           :stderr (get-buffer-create strisper--stderr-buffer-name)
           :coding 'utf-8-unix ; Use -unix for line ending handling if needed
           ;; Define the filter directly here
           :filter (lambda (process string)
                     (with-current-buffer (process-buffer process)
                       (let ((start (point-max))) ; Record end of buffer before insert
                         (goto-char start)
                         (insert string)
                         ;; Process completed lines from the start up to where new string was inserted
                         (goto-char (point-min))
                         (while (re-search-forward "^\\(.*\\)$\\n" start t)
                           (let* ((line-content (match-string 1))
                                  (line-start (match-beginning 0))
                                  (line-end (match-end 0)))
                             ;; Process the line content
                             (let ((text (strisper--parse-line line-content)))
                               (when (> (length text) 0) ; Only process non-empty lines
                                 (with-current-buffer (get-buffer-create strisper--processed-buffer-name)
                                   (save-excursion ; Append to processed buffer
                                     (goto-char (point-max))
                                     (insert text)
                                     (insert "\n")))))
                             ;; Delete the processed line from *this* buffer (stdout)
                             (delete-region line-start line-end)
                             ;; Adjust 'start' marker after deletion
                             (setq start (- start (- line-end line-start)))
                             ;; Reset search position to the start for the next line
                             (goto-char (point-min)))))
                       ;; End of while loop
                       ) ; End with-current-buffer
                     ;; Call the optional hook *after* processing buffer content
                     (when post-process-hook
                       (funcall post-process-hook)))
           ;; Consider adding a sentinel if you need to know when the process ends
           ;; :sentinel 'my-process-sentinel-function
           ))
    ;; Return the created process object
    strisper--rec-proc))

;; Make inserter use the dedicated variable for target buffer
(defun strisper-at-point-inserter ()
  "Inserts the content of the processed buffer into strisper--target-buffer."
  ;; This function is called by the filter hook, not interactive
  (when (and strisper--target-buffer (buffer-live-p strisper--target-buffer))
    (let ((processed-buffer (get-buffer strisper--processed-buffer-name)))
      (when (and processed-buffer (> (with-current-buffer processed-buffer (point-max)) (point-min)))
          (let ((content (with-current-buffer processed-buffer
                           (prog1 (buffer-string) ; Copy content
                             (delete-region (point-min) (point-max)))))) ; Clear buffer
            ;; Insert into the target buffer
            (with-current-buffer strisper--target-buffer
              (save-excursion ; Insert at point
                (insert content)))
            ;; Maybe force redisplay of the target buffer if needed?
            ;; (with-selected-window (get-buffer-window strisper--target-buffer) (redisplay t))
            )))))

;; Update the calling functions to capture context and pass the hook correctly
;;;###autoload
(defun strisper-record ()
  "Starts the recording process."
  (interactive)
  (if (process-live-p strisper--rec-proc)
      (when (yes-or-no-p "Already recording, kill old process?")
        (kill-process strisper--rec-proc)
        ;; Restart after killing
        (setq strisper--target-buffer nil) ; Clear target buffer if any
        (strisper--process-arecord)) ; No hook needed
    ;; Else: Start fresh
    (setq strisper--target-buffer nil)
    (strisper--process-arecord)))

;;;###autoload
(defun strisper-record-at-point ()
  "Starts recording. Inserts processed text into the current buffer."
  (interactive)
  (let ((original-buffer (current-buffer))) ; Capture buffer where command runs
    (if (process-live-p strisper--rec-proc)
        (when (yes-or-no-p "Already recording, kill old process?")
          (kill-process strisper--rec-proc)
          ;; Restart after killing, setting context and hook
          (setq strisper--target-buffer original-buffer)
          (strisper--process-arecord #'strisper-at-point-inserter)) ; Pass inserter function as hook
      ;; Else: Start fresh, setting context and hook
      (setq strisper--target-buffer original-buffer)
      (strisper--process-arecord #'strisper-at-point-inserter))))

;;;###autoload
(defun strisper-stop ()
  "Stops the recording process."
  (interactive)
  (when (process-live-p strisper--rec-proc)
    (message "Stopping strisper recording process...")
    ;; Interrupt might not be enough for the sh -c pipeline, try killing
    (kill-process strisper--rec-proc)
    (setq strisper--rec-proc nil) ; Clear the variable
    (setq strisper--target-buffer nil) ; Clear target buffer
    (message "Strisper recording process stopped.")
    (run-hooks 'strisper-stopped-hook))) ; Optional hook

;; Add a hook variable if desired
(defvar strisper-stopped-hook nil
  "Hook run when strisper-stop successfully stops the process.")

;; Remember to remove the old strisper--process-buffer-line if it's no longer used
;; (fmakunbound 'strisper--process-buffer-line)

;; Remove unused variable strisper--recording-process if strisper--rec-proc is used
;; (makunbound 'strisper--recording-process)

(provide 'strisper)
;;; strisper.el ends here
