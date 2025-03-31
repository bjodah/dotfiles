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

(defun strisper--parse-line (ln)
  "Parses a line from the server, returning just the text."
  (let ((parts (split-string ln " " t)))
    (if (> (length parts) 2)
        (string-join (nthcdr 2 parts) " ")
      "")))

(defun strisper--process-buffer-line ()
  "Processes a single line from the stdout buffer."
  (save-excursion
    (with-current-buffer (get-buffer strisper--stdout-buffer-name)
      (goto-char (point-min))
      (when (re-search-forward "^\\(.*\\)$\\n" nil t)
        (let* ((line (match-string 1))
               (text (strisper--parse-line line)))
          (with-current-buffer (get-buffer-create strisper--processed-buffer-name)
            (insert text)
            (insert "\n"))
          (delete-region (line-beginning-position) (line-end-position 2)))
        ))))

(defun strisper--process-arecord (&optional filterfunc)
  "Start a recording process in the background."
  (let ((buffer (get-buffer-create strisper--stdout-buffer-name)))
    (set-process-filter
     (setq strisper--rec-proc
           (make-process
            :name "strisper--arecord"
            :command `("sh" "-c" "arecord -f S16_LE -c1 -r 16000 -t raw -D default | nc localhost 43007")
            :buffer buffer
            :stderr (get-buffer-create strisper--stderr-buffer-name)
            :coding 'utf-8
            :filter (lambda (process string)
                      (with-current-buffer (process-buffer process)
                        (insert string)
                        (while (string-match "\\(\\n\\)" (buffer-string))
                          (strisper--process-buffer-line)
                          (redisplay))
                         (when filterfunc
                           (funcall filterfunc))))))
     (lambda (process string)
       (with-current-buffer (process-buffer process)
         (insert string)
         (redisplay)))))

(defun strisper-at-point-inserter ()
  "Inserts the content of the processed buffer at point."
  (interactive)
  (let ((buffer (get-buffer strisper--processed-buffer-name)))
    (when buffer
      (save-excursion
        (with-current-buffer buffer
          (goto-char (point-min))
          (insert-buffer-substring buffer (point-min) (point-max))
          (delete-region (point-min) (point-max)))))))


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
    (strisper--process-arecord #'strisper-at-point-inserter)))


;;;###autoload
(defun strisper-stop ()
  "Stops the recording process."
  (interactive)
  (when (process-live-p strisper--rec-proc)
    (interrupt-process strisper--rec-proc)
  ))
  

(provide 'strisper)
;;; strisper.el ends here

