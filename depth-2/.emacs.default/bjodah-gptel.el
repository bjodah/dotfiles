(use-package gptel
  :vc (:url "https://github.com/karthink/gptel")
  :ensure t
  :config
  (setq
   gptel-model 'gemini-2.0-flash
   gptel-backend (gptel-make-gemini "Gemini"
                   :key "GEMINI_API_KEY" ;(lambda () (shell-command-to-string "cat ~/doc/it/*nycklar*/g-gmni.* | tail -c+19 | head -c 39"))
                   :stream t))
  (setq gptel-temperature 0.2)
  :bind (("C-c ." . 'gptel-send)
         ("C-c /" . 'gptel-rewrite)
         ("C-c M->" . (lambda ()
                      (interactive)
                      (let ((buffer (get-buffer "*gptel*")))
                        (if buffer
                            (let ((window (get-buffer-window buffer)))
                              (if window
                                  (select-window window)
                                (gptel "*gptel*")
                                ))
                          (gptel "*gptel*"))
                        (let ((window (get-buffer-window "*gptel*")))
                          (if window (select-window window)
                            (switch-to-buffer "*gptel*")))))))
  )


(use-package gptel-quick
  :vc (:url "https://github.com/karthink/gptel-quick")
  :ensure t
  :after embark
  :config
  (keymap-set embark-region-map "?" #'gptel-quick)
  ;;:bind (("C-c ?" . gptel-quick)) ; C-. ?
)



(gptel-make-openai "Groq"
  :host "api.groq.com"
  :endpoint "/openai/v1/chat/completions"
  :stream t
  :key "GROQ_API_KEY" ;(lambda () (shell-command-to-string "cat ~/doc/it/*nycklar*/grq-min-nyckel-16feb.txt | tail -c+19 | head -c -6"))

  :models '(;deepseek-r1-distill-llama-70b-specdec <-- use browser: https://inference.cerebras.ai/
            llama-3.3-70b-specdec
            qwen-2.5-coder-32b
            qwen-qwq-32b
            ;gemma2-9b-it
            ;llama-3.3-70b-versatile
            ;llama-3.1-8b-instant
            ;llama-3.2-3b-preview

            ))

;; xAI offers an OpenAI compatible API
(gptel-make-openai "xAI"           ;Any name you want
  :host "api.x.ai"
  :key "XAI_API_KEY" ;(lambda () (shell-command-to-string "cat ~/doc/it/*nycklar*/xai-2025-feb.*"))
  :endpoint "/v1/chat/completions"
  :stream t
  :models '(;; xAI now only offers `grok-beta` as of the time of this writing
            grok-beta))

(gptel-make-openai "DeepSeek"       ;Any name you want
  :host "api.deepseek.com"
  :endpoint "/chat/completions"
  :stream t
  :key "DEEPSEEK_API_KEY" ; (lambda () (shell-command-to-string "cat ~/doc/it/*nycklar*/dpsk-2025-feb.*"))
  :models '(deepseek-chat ; v3
            deepseek-reasoner ; r1
            ))


(gptel-make-openai "localhost-8000"
  :stream t
  :protocol "http"
  :host "localhost:8000"
  :key "duck123" ;(lambda () (shell-command-to-string "cat ~/doc/it/*nycklar*/vllm-local-api-key.txt"))
  ;:models '(Qwen/Qwen2.5-Coder-32B-Instruct-AWQ)
  ;:models '(stelterlab/phi-4-AWQ)
  :models '(TabbyAPI-QwenCoder14B)
)
