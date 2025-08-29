(use-package gptel
  :vc (:url "https://github.com/karthink/gptel"
            :rev :newest
            :branch "master")
  :ensure t
  :config
  (setq
   gptel-api-key (lambda () (getenv "OPENAI_API_KEY"))
   gptel-model 'llamacpp-Qwen3-Coder-30B-A3B-it ; 'gemini-2.5-flash-preview-05-20
   ;gptel-backend ...
   )
  ;(setq gptel-temperature 0.2)
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
  :bind (("C-c M-," . gptel-quick)) ; C-. ?
)

;; (gptel-make-gemini "Gemini"
;;   :key (lambda () (getenv "GEMINI_API_KEY"))
;;   :models '(gemini-2.5-flash-preview-05-20)
;;   :stream t)

(gptel-make-openai "Groq"
  :host "api.groq.com"
  :endpoint "/openai/v1/chat/completions"
  :stream t
  :key (lambda () (getenv "GROQ_API_KEY"))
  ;; https://console.groq.com/docs/models
  ;; https://console.groq.com/docs/deprecations
  ;; https://console.groq.com/dashboard/limits
  :models '(
            openai/gpt-oss-20b
            openai/gpt-oss-120b
            moonshotai/kimi-k2-instruct ; 131 072, 16 384
            meta-llama/llama-4-maverick-17b-128e-instruct ; 131 072, 8192
            meta-llama/llama-4-scout-17b-16e-instruct ; 131 072, 8192
            llama-3.3-70b-versatile ; 128k, 32768
            llama-3.1-8b-instant ; 128k, 8192
            ;qwen-qwq-32b ; 128l, -
            ;deepseek-r1-distill-qwen-32b ; 128k, 16384
            ;deepseek-r1-distill-llama-70b ; 128k, -
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
  :models '(gpt-oss-120b
            qwen-3-235b-a22b
            llama3.3-70b
            llama3.1-8b
            qwen-3-32b
            llama-4-scout-17b-16e-instruct))

(gptel-make-openai "Alibaba"
  :stream t
  :key (lambda () (getenv "ALIBABA_API_KEY"))
  :protocol "https"
  :host "dashscope-intl.aliyuncs.com"
  :endpoint "/compatible-mode/v1/chat/completions"
  :models '(qwen3-coder-plus))

(gptel-make-openai "OpenRouter"               ;Any name you want
  :host "openrouter.ai"
  :endpoint "/api/v1/chat/completions"
  :stream t
  :key (lambda () (getenv "OPENROUTER_API_KEY"))
  :models '(qwen/qwen3-235b-a22b-2507
            ;qwen/qwen3-235b-a22b:free
            qwen/qwen2.5-coder-7b-instruct ; 33k, 33k, $0.01/$0.03 ~160tps (nebius)
            mistralai/devstral-medium
            mistralai/mistral-medium-3
            z-ai/glm-4.5 ; 129k, 96k, $0.6/2.2
            z-ai/glm-4.5-air ; 129k, 96k, $0.2/1.1
            ;anthropic/claude-opus-4 ; 200k, 32k, $15/$75, ~30tps
            anthropic/claude-sonnet-4 ; 200k, 64k, $3/$15, ~60tps
            ;anthropic/claude-3.7-sonnet
            ))

;; (gptel-make-openai "localhost-8000"
;;   :stream t
;;   :protocol "http"
;;   :host "localhost:8000"
;;   :key "duck123"
;;   :models '(TabbyAPI-QwenCoder14B)
;; )
(require 'gptel)
(defun bjodah/gptel-spot-typos-in-region ()
  "Query gptel with the selected region to find bugs or typos.

The response is displayed in the *gptel-typo-spotting* buffer."
  (interactive)

  ;; 1. Ensure a region is selected.
  (unless (use-region-p)
    (user-error "You must select a region of text first"))

  ;; 2. Get the text from the selected region.
  (let* ((selected-text (buffer-substring-no-properties (region-beginning) (region-end)))
         (prompt (concat "Can you find any potential bugs or typos in this text?:\n\n"
                         selected-text)))

    (message "Asking gptel to check for bugs and typos...")

    ;; 3. Call gptel-request with a hard-coded model and a custom callback.
    (let ((gptel-model 'llamacpp-Qwen3-Coder-30B-A3B-it))
      (gptel-request
       prompt
       ;; 4. The callback function defines what to do with the response.
       :callback
       (lambda (response info)
         ;; The `response` argument contains the text from the LLM.
         ;; We only act if the response is a valid string.
         (when (stringp response)
           (with-current-buffer (get-buffer-create "*gptel-typo-spotting*")
             ;; Prepare and populate the temporary buffer.
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert response)
               ;; Use markdown-mode for better formatting if it's available.
               (when (fboundp 'markdown-mode)
                 (markdown-mode))
               (goto-char (point-min)))
             ;; Display the buffer in another window.
             (display-buffer (current-buffer) t))))))))

(defun bjodah--generate-gptel-conf ()
  (insert (shell-command-to-string (format "%s %s gptel --port 8688"
                                   (expand-file-name "~/venv/bin/python")
                                   (expand-file-name "~/vc/llm-multi-backend-container/scripts/generate-emacs-configs.py")
                                   ))))
(setq gptel-backend     
;; the contents below is generated by the command above, <generated_text>
(gptel-make-openai "llm-multi-backend"
  :stream t
  :protocol "http"
  :host (concat (if (string= (getenv "container") "podman") "host.docker.internal" "localhost") ":8688")
  :key "sk-empty"
  :models '(exllamav2-QwQ-32B
            exllamav2-QwQ-32B@think-less
            exllamav2-Qwen2.5-Coder-14B
            exllamav2-Qwen2.5-Coder-7B
            exllamav2-gemma-3-27b
            llamacpp-Broken-Tutu-24B
            llamacpp-DeepCoder-14B
            llamacpp-Forgotten-Safeword-36B-4.1
            llamacpp-Ling-Coder-lite
            llamacpp-Omega-Darker-Gaslight_The-Final-Forgotten-Fever-Dream-24B
            llamacpp-Omega-Darker-final-directive-24b
            llamacpp-Omega-Directive-24B-Unslop
            llamacpp-Phi-4
            llamacpp-Phi-4-ablt
            llamacpp-Phi-4-ablt-Orion-18B
            llamacpp-Phi-4-reasoning-plus
            llamacpp-QwQ-32B
            llamacpp-QwQ-32B-RpR
            llamacpp-QwQ-32B-RpR@think-less
            llamacpp-QwQ-32B@think-less
            llamacpp-Qwen2.5-Coder-14B
            llamacpp-Qwen2.5-Coder-32B
            llamacpp-Qwen2.5-Coder-3B
            llamacpp-Qwen2.5-Coder-7B
            llamacpp-Qwen3-0.6B
            llamacpp-Qwen3-0.6B@do-think
            llamacpp-Qwen3-0.6B@no-think
            llamacpp-Qwen3-0.6B@think-silent
            llamacpp-Qwen3-1.7B
            llamacpp-Qwen3-1.7B@do-think
            llamacpp-Qwen3-1.7B@no-think
            llamacpp-Qwen3-1.7B@think-silent
            llamacpp-Qwen3-14B
            llamacpp-Qwen3-14B@do-think
            llamacpp-Qwen3-14B@no-think
            llamacpp-Qwen3-14B@think-silent
            llamacpp-Qwen3-30B-A3B
            llamacpp-Qwen3-30B-A3B-K_XL
            llamacpp-Qwen3-30B-A3B-K_XL@do-think
            llamacpp-Qwen3-30B-A3B-K_XL@no-think
            llamacpp-Qwen3-30B-A3B-K_XL@think-silent
            llamacpp-Qwen3-30B-A3B-cpu
            llamacpp-Qwen3-30B-A3B-cpu@do-think
            llamacpp-Qwen3-30B-A3B-cpu@no-think
            llamacpp-Qwen3-30B-A3B-cpu@think-silent
            llamacpp-Qwen3-30B-A3B-it-2507
            llamacpp-Qwen3-30B-A3B-think-2507
            llamacpp-Qwen3-30B-A3B-think-2507@do-think
            llamacpp-Qwen3-30B-A3B-think-2507@no-think
            llamacpp-Qwen3-30B-A3B-think-2507@think-silent
            llamacpp-Qwen3-30B-A3B@do-think
            llamacpp-Qwen3-30B-A3B@no-think
            llamacpp-Qwen3-30B-A3B@think-silent
            llamacpp-Qwen3-32B
            llamacpp-Qwen3-32B@do-think
            llamacpp-Qwen3-32B@no-think
            llamacpp-Qwen3-32B@think-silent
            llamacpp-Qwen3-4B
            llamacpp-Qwen3-4B-128K
            llamacpp-Qwen3-4B-128K@do-think
            llamacpp-Qwen3-4B-128K@no-think
            llamacpp-Qwen3-4B-128K@think-silent
            llamacpp-Qwen3-4B@do-think
            llamacpp-Qwen3-4B@no-think
            llamacpp-Qwen3-4B@think-silent
            llamacpp-Qwen3-8B
            llamacpp-Qwen3-8B@do-think
            llamacpp-Qwen3-8B@no-think
            llamacpp-Qwen3-8B@think-silent
            llamacpp-Qwen3-Coder-30B-A3B-it
            llamacpp-Qwen3-Coder-30B-A3B-it@do-think
            llamacpp-Qwen3-Coder-30B-A3B-it@no-think
            llamacpp-Qwen3-Coder-30B-A3B-it@think-silent
            llamacpp-amoral-gemma-3-27b
            llamacpp-cydonia-24b-v4.1
            llamacpp-devstral-small-2507
            llamacpp-exaone-deep-32b
            llamacpp-fallen-gemma-3-27b
            llamacpp-gemma-3-12b
            llamacpp-gemma-3-1b
            llamacpp-gemma-3-270m-it
            llamacpp-gemma-3-270m-it-qat
            llamacpp-gemma-3-27b
            llamacpp-gemma-3-4b
            llamacpp-glm-4-32b-0414
            llamacpp-glm-4.5-air
            llamacpp-glm-4.5-air-ablt
            llamacpp-glm-4.5-air-ablt@extract-thinking
            llamacpp-glm-z1-32b-0414
            llamacpp-glm-z1-rumination-32b-0414
            llamacpp-gpt-oss-120b
            llamacpp-gpt-oss-120b@high
            llamacpp-gpt-oss-120b@low
            llamacpp-gpt-oss-120b@medium
            llamacpp-gpt-oss-20b
            llamacpp-gpt-oss-20b@high
            llamacpp-gpt-oss-20b@low
            llamacpp-gpt-oss-20b@medium
            llamacpp-lars1234-mistral-small-24b-2501-writer
            llamacpp-ling-lite-0415
            llamacpp-llama4-maverick
            llamacpp-magistral-small-2507
            llamacpp-mistral-small-24b-2501
            llamacpp-mistral-small-3.1-24b-2503
            llamacpp-mistral-small-3.2-24b-2506
            llamacpp-openthinker2-32b
            llamacpp-qwen25-32b-ablt
            llamacpp-seed-oss-36b
            llamacpp-seed-oss-36b@extract-thinking
            llamacpp-tiger-gemma-v3-27b
            vllm-Qwen-QwQ-32B
            vllm-Qwen-QwQ-32B@think-less
            vllm-Qwen2.5-Coder-14B
            vllm-Qwen2.5-Coder-7B
            vllm-Qwen2.5-VL-7B
            vllm-Qwen3-14B
            vllm-Qwen3-14B@do-think
            vllm-Qwen3-14B@no-think
            vllm-Qwen3-14B@think-silent
            vllm-SmolLM2-1.7B-Instruct)
)
;; </generated_text>
)

(pcase (system-name)
  ("yoga720"
   (gptel-make-openai "oai8685"
     :stream t
     :protocol "http"
     :host "localhost:8685"
     :key "sk-empty"
     :models '(port8685))
   ))
