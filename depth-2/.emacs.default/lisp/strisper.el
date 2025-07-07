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

;; Keep the target buffer variable
(defvar strisper--target-buffer nil
  "Buffer where strisper-record-at-point should insert text.")

;; Keep the logging buffers
(defvar strisper--stdout-buffer-name "*strisper-stdout*")
(defvar strisper--stderr-buffer-name "*strisper-stderr*")
(defvar strisper--processed-log-buffer-name "*strisper-processed-log*")

;; Keep the process variable
(defvar strisper--rec-proc nil)

;; Modify the process creation function with explicit buffer switching
(defun strisper--process-arecord ()
  "Start a recording process in the background.
Inserts parsed text directly into strisper--target-buffer if set."
  (let ((stdout-buffer (get-buffer-create strisper--stdout-buffer-name)))
    (setq strisper--rec-proc
          (make-process
           :name "strisper--arecord"
           :command '("sh" "-c" "arecord -f S16_LE -c1 -r 16000 -t raw -D pulse | nc localhost 43007")
           :buffer stdout-buffer ; Process output goes here by default
           :stderr (get-buffer-create strisper--stderr-buffer-name)
           :coding 'utf-8-unix
           ;; --- Modified Filter with Explicit Switching ---
           :filter (lambda (process string)
                     (let ((match (string-match "^\\([0-9]+\\)[[:space:]]+\\([0-9]+\\)[[:space:]]+\\(.*\\)$" string)))
                       (when (and match
                                  strisper--target-buffer
                                  (buffer-live-p strisper--target-buffer))
                         (with-current-buffer strisper--target-buffer
                          (insert (string-replace "  " " " (match-string 3 string)))
                          (insert " ")
                          )))
                     (with-current-buffer (process-buffer process)
                       (insert string)))
           ))) ; End make-process
    strisper--rec-proc)

;; --- Calling functions remain the same ---

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

;;;### autoload
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
