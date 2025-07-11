(use-package gptel
  :vc (:url "https://github.com/karthink/gptel"
            :rev :newest
            :branch "master")
  :ensure t
  :config
  (setq
   gptel-api-key (lambda () (getenv "OPENAI_API_KEY"))
   gptel-model 'gemini-2.5-flash-preview-05-20
   gptel-backend (gptel-make-gemini "Gemini"
                   :key (lambda () (getenv "GEMINI_API_KEY"))
                   :stream t))
  (setq gptel-temperature 0.2)
  :bind (("C-c M-." . 'gptel-send)
         ("C-c M-/" . 'gptel-rewrite)
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
  :vc (:url "https://github.com/karthink/gptel-quick"
            :rev :newest
            :branch "master")
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
  :models '(
            meta-llama/llama-4-maverick-17b-128e-instruct ; 131 072, 8192
            meta-llama/llama-4-scout-17b-16e-instruct ; 131 072, 8192
            llama-3.3-70b-versatile ; 128k, 32768
            llama-3.1-8b-instant ; 128k, 8192
            qwen-qwq-32b ; 128l, -
            deepseek-r1-distill-qwen-32b ; 128k, 16384
            deepseek-r1-distill-llama-70b ; 128k, -
            compound-beta; 128k, 8192  (searches web, ...)
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
            ;grok-3-fast-beta
            ))

(gptel-make-openai "DeepSeek"       ;Any name you want
  :host "api.deepseek.com"
  :endpoint "/chat/completions"
  :stream t
  :key (lambda () (getenv "DEEPSEEK_API_KEY"))
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
            qwen-3-32b
            llama-4-scout-17b-16e-instruct))

(gptel-make-openai "OpenRouter"               ;Any name you want
  :host "openrouter.ai"
  :endpoint "/api/v1/chat/completions"
  :stream t
  :key (lambda () (getenv "OPENROUTER_API_KEY"))
  :models '(qwen/qwen3-235b-a22b
            qwen/qwen3-235b-a22b:free
            qwen/qwen2.5-coder-7b-instruct ; 33k, 33k, $0.01/$0.03 ~160tps (nebius)
            mistralai/mistral-medium-3
            ;anthropic/claude-opus-4 ; 200k, 32k, $15/$75, ~30tps
            anthropic/claude-sonnet-4 ; 200k, 64k, $3/$15, ~60tps
            anthropic/claude-3.7-sonnet))

;; (gptel-make-openai "localhost-8000"
;;   :stream t
;;   :protocol "http"
;;   :host "localhost:8000"
;;   :key "duck123"
;;   :models '(TabbyAPI-QwenCoder14B)
;; )

(defun bjodah--generate-gptel-conf ()
  (message (shell-command-to-string (format "%s %s %s"
                                   (expand-file-name "~/venv/bin/python")
                                   (expand-file-name "~/vc/llm-multi-backend-container/scripts/generate-emacs-configs.py")
                                   "gptel" "--port" "8688")))
)
;; The contents below is generated by the command above
(gptel-make-openai "llm-multi-backend"
  :stream t
  :protocol "http"
  :host (concat (if (string= (getenv "container") "podman") "host.docker.internal" "localhost") ":8688")
  :key "sk-empty"
  :models '(llamacpp-Qwen2.5-Coder-7B llamacpp-Qwen3-0.6B llamacpp-Qwen3-1.7B llamacpp-Qwen3-32B llamacpp-gemma-3-27b llamacpp-gemma-3-1b llamacpp-qwen25-32b-ablt llamacpp-Broken-Tutu-24B exllamav2-Qwen2.5-Coder-7B vllm-Qwen2.5-Coder-14B llamacpp-Qwen3-4B llamacpp-Qwen3-8B llamacpp-mistral-small-3.1-24b-2503 llamacpp-DeepCoder-14B llamacpp-glm-z1-32b-0414 llamacpp-QwQ-32B-RpR llamacpp-Omega-Darker-Gaslight_The-Final-Forgotten-Fever-Dream-24B llamacpp-Qwen2.5-Coder-32B llamacpp-Qwen3-14B llamacpp-openthinker2-32b llamacpp-Qwen3-4B-128K llamacpp-amoral-gemma-3-27b llamacpp-Phi-4-ablt vllm-Qwen2.5-Coder-7B llamacpp-QwQ-32B vllm-SmolLM2-1.7B-Instruct llamacpp-gemma-3-4b llamacpp-ling-lite-0415 llamacpp-Phi-4-reasoning-plus llamacpp-Qwen2.5-Coder-3B exllamav2-Qwen2.5-Coder-14B vllm-Qwen2.5-VL-7B llamacpp-Ling-Coder-lite llamacpp-Qwen3-30B-A3B llamacpp-lars1234-mistral-small-24b-2501-writer vllm-Qwen3-14B llamacpp-llama4-maverick exllamav2-gemma-3-27b vllm-Qwen-QwQ-32B llamacpp-gemma-3-12b llamacpp-glm-z1-rumination-32b-0414 llamacpp-Omega-Darker-final-directive-24b exllamav2-QwQ-32B llamacpp-exaone-deep-32b llamacpp-glm-4-32b-0414 llamacpp-fallen-gemma-3-27b llamacpp-Phi-4-ablt-Orion-18B llamacpp-Phi-4 llamacpp-Qwen2.5-Coder-14B llamacpp-Qwen3-30B-A3B-cpu llamacpp-mistral-small-24b-2501 llamacpp-devstral-small-2505 llamacpp-cydonia-24b-v2.1 llamacpp-Qwen3-0.6B@think llamacpp-Qwen3-0.6B@no-think llamacpp-Qwen3-0.6B@think-silent llamacpp-Qwen3-1.7B@think llamacpp-Qwen3-1.7B@no-think llamacpp-Qwen3-1.7B@think-silent llamacpp-Qwen3-32B@think llamacpp-Qwen3-32B@no-think llamacpp-Qwen3-32B@think-silent llamacpp-Qwen3-4B@think llamacpp-Qwen3-4B@no-think llamacpp-Qwen3-4B@think-silent llamacpp-Qwen3-8B@think llamacpp-Qwen3-8B@no-think llamacpp-Qwen3-8B@think-silent llamacpp-QwQ-32B-RpR@think-less llamacpp-Qwen3-14B@think llamacpp-Qwen3-14B@no-think llamacpp-Qwen3-14B@think-silent llamacpp-Qwen3-4B-128K@think llamacpp-Qwen3-4B-128K@no-think llamacpp-Qwen3-4B-128K@think-silent llamacpp-QwQ-32B@think-less llamacpp-Qwen3-30B-A3B@think llamacpp-Qwen3-30B-A3B@no-think llamacpp-Qwen3-30B-A3B@think-silent vllm-Qwen3-14B@think vllm-Qwen3-14B@no-think vllm-Qwen3-14B@think-silent vllm-Qwen-QwQ-32B@think-less exllamav2-QwQ-32B@think-less llamacpp-Qwen3-30B-A3B-cpu@think llamacpp-Qwen3-30B-A3B-cpu@no-think llamacpp-Qwen3-30B-A3B-cpu@think-silent)
)
