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

(defvar strisper--target-buffer nil
  "Buffer where strisper-record-at-point should insert text.")
(defvar strisper--stdout-buffer-name "*strisper-stdout*")
(defvar strisper--stderr-buffer-name "*strisper-stderr*")
(defvar strisper--processed-log-buffer-name "*strisper-processed-log*") ; Renamed for clarity
(defvar strisper--rec-proc nil)

(defun strisper--parse-line (ln)
  "Parses a line from the server, returning just the text."
  (let ((parts (split-string ln "[[:space:]]+" t)))
    (if (> (length parts) 2)
        (string-join (nthcdr 2 parts) " ")
      "")))

(defun strisper--process-arecord ()
  "Start a recording process in the background.
Inserts parsed text directly into strisper--target-buffer if set."
  (let ((stdout-buffer (get-buffer-create strisper--stdout-buffer-name)))
    (setq strisper--rec-proc
          (make-process
           :name "strisper--arecord"
           :command '("sh" "-c" "arecord -f S16_LE -c1 -r 16000 -t raw -D default | nc localhost 43007")
           :buffer stdout-buffer
           :stderr (get-buffer-create strisper--stderr-buffer-name)
           :coding 'utf-8-unix
           ;; --- Modified Filter ---
           :filter (lambda (process string)
                     (with-current-buffer (process-buffer process) ; Should be stdout-buffer
                       (message string);;<---DEBUG, This prints the unparsed incoming line in 
                       (message (process-buffer process));;<--DEBUG, 
                       (let ((start (point-max))
                             (end-marker (make-marker))) ; Use marker robustly
                         (goto-char start)
                         (insert string)
                         (set-marker end-marker (point)) ; Mark end of newly inserted text

                         ;; Process completed lines from the start up to the end marker
                         (goto-char (point-min))
                         (while (re-search-forward "^\\(.*\\)$\\n" end-marker t)
                           (let* ((line-content (match-string 1))
                                  (line-start (match-beginning 0))
                                  (line-end (match-end 0))
                                  ;; --- PARSE THE LINE ---
                                  (parsed-text (strisper--parse-line line-content)))

                             ;; --- INSERT DIRECTLY if text exists and target is valid ---
                             (when (and (> (length parsed-text) 0)
                                        strisper--target-buffer
                                        (buffer-live-p strisper--target-buffer))
                               (with-current-buffer strisper--target-buffer
                                 (save-excursion
                                   ;; Insert at point in the target buffer
                                   (insert parsed-text)
                                   (insert " ") ; Add a space after inserted text? Optional.
                                   )))
                               ;; Force redisplay immediately? Might be slow.
                               ;; (redisplay t)

                             ;; Optional: Log processed text
                             (when (> (length parsed-text) 0)
                               (with-current-buffer (get-buffer-create strisper--processed-log-buffer-name)
                                 (save-excursion
                                   (goto-char (point-max))
                                   (insert parsed-text)
                                   (insert "\n"))))

                             ;; --- Delete the processed line from *this* buffer (stdout) ---
                             (delete-region line-start line-end)
                             ;; Adjust end marker position
                             (set-marker end-marker (- (marker-position end-marker) (- line-end line-start)))
                             ;; Reset search position for next line
                             (goto-char (point-min))))
                         ;; End while loop
                         (set-marker end-marker nil) ; Clean up marker
                         )) ; End with-current-buffer
                       ) ; End filter lambda
           ;; Add sentinel?
           ;; :sentinel 'my-process-sentinel-function
           ))
    strisper--rec-proc))

;;;###autoload
(defun strisper-record ()
  "Starts the recording process. Output logged to internal buffers."
  (interactive)
  (if (process-live-p strisper--rec-proc)
      (when (yes-or-no-p "Already recording, kill old process?")
        (strisper-stop) ; Use stop function for cleanup
        (setq strisper--target-buffer nil)
        (strisper--process-arecord))
    ;; Else: Start fresh
    (setq strisper--target-buffer nil)
    (strisper--process-arecord)))

;;;###autoload
(defun strisper-record-at-point ()
  "Starts recording. Inserts processed text into the current buffer."
  (interactive)
  (message "starting at point");;<---DEBUG, remove this line

  (let ((original-buffer (current-buffer)))
    (if (process-live-p strisper--rec-proc)
        (when (yes-or-no-p "Already recording, kill old process?")
          (strisper-stop) ; Use stop function for cleanup
          ;; Restart, setting context
          (setq strisper--target-buffer original-buffer)
          (strisper--process-arecord))
      ;; Else: Start fresh, setting context
      (setq strisper--target-buffer original-buffer)
      (strisper--process-arecord))))

;;;###autoload
(defun strisper-stop ()
  "Stops the recording process."
  (interactive)
  (when (process-live-p strisper--rec-proc)
    (message "Stopping strisper recording process...")
    (kill-process strisper--rec-proc)
    (setq strisper--rec-proc nil)
    (setq strisper--target-buffer nil)
    (message "Strisper recording process stopped.")
    ;; (run-hooks 'strisper-stopped-hook) ; Optional
    ))

(provide 'strisper)
;;; strisper.el ends here
