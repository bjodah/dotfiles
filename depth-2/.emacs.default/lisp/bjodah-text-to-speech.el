;;; bjodah-text-to-speech.el --- Text-to-Speech using API endpoint -*- lexical-binding: t; -*-
(require 'json)
(defun bjodah-text-to-speech ()
  "Send the currently marked selection to a local TTS server and play audio non-blocking.

Requires `curl` and `ffplay` to be installed and in your PATH.
Assumes a local server is running at http://localhost:8880/v1/audio/speech.
Uses 'af_sky' voice by default.

To use:
1. Mark the text you want to convert to speech.
2. Execute this function, e.g., with `M-x bjodah-text-to-speech` or bind it to a key."
  (interactive)
  (when (region-active-p)
    (let ((selected-text (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))))
      (when (string-empty-p selected-text)
        (message "Selection is empty."))
      (unless (string-empty-p selected-text)
        (let* ((json-payload `((input . ,selected-text)
                               (voice . "af_sky+af_bella")))
               (json-string (json-encode json-payload))
               (temp-json-file (make-temp-file "my-tts" nil ".json")))
          (message json-string)
          (message temp-json-file)
          (write-region json-string nil temp-json-file)
          (let ((curl-command
                 (format "curl -X POST http://localhost:8880/v1/audio/speech -H 'Content-Type: application/json' -d @%s | ffplay -nodisp -autoexit -loglevel quiet - ; rm %s"
                         (shell-quote-argument temp-json-file) (shell-quote-argument temp-json-file))))
            (message "Executing text-to-speech in the background...")
            (async-shell-command curl-command "*TTS curl output*") ; Using async-shell-command for non-blocking execution
            (message (format "Text-to-speech command started in the background: %s" curl-command)))
          ))))
  (unless (region-active-p)
    (message "No region selected. Please mark the text you want to convert to speech.")))

(provide 'bjodah-text-to-speech)
