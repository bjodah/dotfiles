(use-package gptel
  :vc (:url "https://github.com/karthink/gptel"
            :rev :newest
            :branch "master")
  :ensure t
  :config
  (setq
   gptel-model 'gemini-2.0-flash ; gemini-2.5-flash-preview-04-17
   gptel-backend (gptel-make-gemini "Gemini"
                   :key (lambda () (getenv "GEMINI_API_KEY"))
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
  :key (lambda () (getenv "GROQ_API_KEY"))
  ;; https://console.groq.com/docs/models
  ;; https://console.groq.com/docs/deprecations
  ;; https://console.groq.com/dashboard/limits
  :models '(llama-3.1-8b-instant ; 128k, 8.192
            meta-llama/llama-4-scout-17b-16e-instruct ; 131 072, 8192
            meta-llama/llama-4-maverick-17b-128e-instruct ; 131 072, 8192
            qwen-qwq-32b ; 128l, -
            deepseek-r1-distill-qwen-32b ; 128k, 16384
            deepseek-r1-distill-llama-70b ; 128k, -
            ))

;; xAI offers an OpenAI compatible API
(gptel-make-openai "xAI"           ;Any name you want
  :host "api.x.ai"
  :key (lambda () (getenv "XAI_API_KEY"))
  :endpoint "/v1/chat/completions"
  :stream t
  ;; https://console.x.ai/ ;;
  :models '(grok-3-mini-beta
            grok-3-beta
            grok-3-fast-beta))

(gptel-make-openai "DeepSeek"       ;Any name you want
  :host "api.deepseek.com"
  :endpoint "/chat/completions"
  :stream t
  :key (lambda () (getenv "DEEPSEEK_API_KEY")) ; (lambda () (shell-command-to-string "cat ~/doc/it/*nycklar*/dpsk-2025-feb.*"))
  :models '(deepseek-chat ; v3
            deepseek-reasoner ; r1
            ))

(gptel-make-openai "Cerebras"
  :host "api.cerebras.ai"
  :endpoint "/v1/chat/completions"
  :stream nil                                    ;optionally nil as Cerebras is instant AI
  :key (lambda () (getenv "CEREBRAS_API_KEY")) ;can be a function that returns the key
  :models '(llama3.3-70b
            llama3.1-8b
            llama-4-scout-17b-16e-instruct))


;; (gptel-make-openai "localhost-8000"
;;   :stream t
;;   :protocol "http"
;;   :host "localhost:8000"
;;   :key "duck123"
;;   :models '(TabbyAPI-QwenCoder14B)
;; )

(gptel-make-openai "llama-swap-gemma3"
  :stream t
  :protocol "http"
  :host "localhost:8686"
  :key "sk-empty"
  ;; ~/vc/llm-multi-backend-container/configs/llama-swap-config.yaml
  :models '(llamacpp-gemma-3-27b-it
            exllamav2-Qwen2.5-Coder-14B-Instruct)
)
