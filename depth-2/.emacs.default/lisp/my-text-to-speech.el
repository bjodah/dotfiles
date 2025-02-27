;;; my-text-to-speech.el --- Text-to-Speech using API endpoint -*- lexical-binding: t; -*-

(defun my-text-to-speech-non-blocking ()
  "Send the currently marked selection to a local TTS server and play audio non-blocking.

Requires `curl` and `ffplay` to be installed and in your PATH.
Assumes a local server is running at http://localhost:8880/v1/audio/speech.
Uses 'af_sky' voice by default.

To use:
1. Mark the text you want to convert to speech.
2. Execute this function, e.g., with `M-x my-text-to-speech-non-blocking` or bind it to a key."
  (interactive)
  (when (region-active-p)
    (let ((selected-text (buffer-substring-no-properties (region-beginning) (region-end))))
      (when (string-empty-p selected-text)
        (message "Selection is empty."))
      (unless (string-empty-p selected-text)
        (let ((curl-command
                (format "curl -X POST http://localhost:8880/v1/audio/speech -H 'Content-Type: application/json' -d %s | ffplay -nodisp -autoexit -loglevel quiet -"
                        (json-encode-string (json-encode
                         `((input . ,selected-text)
                           (voice . "af_sky")))))))
          (message "Executing text-to-speech in the background...")
          (async-shell-command curl-command "*TTS curl output*") ; Using async-shell-command for non-blocking execution
          (message (format "Text-to-speech command started in the background: %s" curl-command))))))
  (unless (region-active-p)
    (message "No region selected. Please mark the text you want to convert to speech.")))

(provide 'my-text-to-speech-non-blocking)
