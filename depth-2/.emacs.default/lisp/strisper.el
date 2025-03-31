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
(defvar strisper--stdout-buffer-name "*strisper-processed*")
(defvar strisper--rec-proc nil)

(defun strisper--parse-line (ln)
  ;; ln is a string of the format "<integer> <integer> <string>"
  ;; this function discards the two integers and returns the string content.
  ;; TODO: implement
  )

(defun strisper--process-arecord (&optional filterfunc)
  "Start a recording process in the background."
  (setq strisper--rec-proc
        (make-process
         :name "strisper--arecord"
         :command `("sh" "-c" "arecord -f S16_LE -c1 -r 16000 -t raw -D default | nc localhost 43007")
         :buffer (get-buffer-create strisper--stdout-buffer-name)
         :stderr (get-buffer-create strisper--stderr-buffer-name)
         :coding 'utf-8
         ))
  ;; strisper--rec-proc now streams lines into the buffer named strisper--stdout-buffer-name
  ;; The lines look like follows, two integers (timestamps) and then the text:
  ;;
  ;;    0 160  Thank  you.
  ;;    2340 3340  Thank  you  for  watching!

  ;; TODO: If filterfunc arg was provided, it is registered to process each line that is
  ;; streamed into strisper--stdout-buffer-name
  )

(defun stripser-at-point-inserter
    ;;TODO implement)
  

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
(defun strisper-record-at-point ()
  "Starts the recording process. Continously inserts processed text from striper's output buffer."
  (interactive)
  (if (process-live-p strisper--rec-proc)
      (when (yes-or-no-p "Already recording, kill old process?")
        (kill-process strisper--rec-proc))
    (strisper--process-arecord strisper--at-point-inserter)))


;;;###autoload
(defun strisper-stop ()
  "Stops the recording process."
  (interactive)
  (when (process-live-p strisper--rec-proc)
    (interrupt-process strisper--rec-proc)
  ))
  

(provide 'strisper)
;;; strisper.el ends here
