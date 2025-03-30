;;; strisper.el --- Streaming Speech-to-Text using whipser -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Björn Ingvar Dahlgren

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
(defvar strisper--rec-proc nil)


(defun strisper--process-arecord ()
  "Start a recording process in the background."
  (setq strisper--rec-proc
        (make-process
         :name "strisper--arecord"
         :command `("sh" "-c" "arecord -f S16_LE -c1 -r 16000 -t raw -D default | nc localhost 43007")
         :buffer (get-buffer-create strisper--stdout-buffer-name)
         :stderr (get-buffer-create strisper--stderr-buffer-name)
         :coding 'utf-8
         )))
  

(defvar strisper--recording-process nil)

;;;###autoload
(defun strisper-record ()
  "Starts the recording process."
  (interactive)
  (if (process-live-p strisper--rec-proc)
      (when (yes-or-no-p "Already recording, kill old process?")
        (kill-process strisper--rec-proc))
    (strisper--process-arecord)))

;;;###autoload
(defun strisper-stop ()
  "Stops the recording process."
  (interactive)
  (when (process-live-p strisper--rec-proc)
    (interrupt-process strisper--rec-proc)
  ))
  

(provide 'strisper)
;;; strisper.el ends here
