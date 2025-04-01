(defun bjodah/minuet-use-groq-llama ()
    ;; GROQ for speed:
    (plist-put minuet-openai-compatible-options :end-point "https://api.groq.com/openai/v1/chat/completions")
    (plist-put minuet-openai-compatible-options :api-key (lambda () (getenv "GROQ_API_KEY")))
    (plist-put minuet-openai-compatible-options :model "llama-3.3-70b-specdec")

    ;; Prioritize throughput for faster completion
    ;(minuet-set-optional-options minuet-openai-compatible-options :provider '(:sort "throughput"))
    (minuet-set-optional-options minuet-openai-compatible-options :max_tokens 256)
    (minuet-set-optional-options minuet-openai-compatible-options :top_p 0.9)
    (setq minuet-provider 'openai-compatible)
    )

(defun bjodah/minuet-use-deepseek ()
  (interactive)
  (setq minuet-openai-fim-compatible-options
        '(:model "deepseek-chat"
    :end-point "https://api.deepseek.com/beta/completions"
    :api-key "DEEPSEEK_API_KEY"
    :name "Deepseek"
    :template (:prompt minuet--default-fim-prompt-function
               :suffix minuet--default-fim-suffix-function)
    :optional nil)) 
  (setq minuet-provider 'openai-fim-compatible)
  )

(defun bjodah/minuet-use-gemini ()
  "Switch minuet provider to Gemini"
  (interactive)
    (setq minuet-provider 'gemini)
    (defvar mg-minuet-gemini-prompt
        "You are the backend of an AI-powered code completion engine. Your task is to
provide code suggestions based on the user's input. The user's code will be
enclosed in markers:
- `<contextAfterCursor>`: Code context after the cursor
- `<cursorPosition>`: Current cursor location
- `<contextBeforeCursor>`: Code context before the cursor
")
    (defvar mg-minuet-gemini-chat-input-template
        "{{{:language-and-tab}}}
<contextBeforeCursor>
{{{:context-before-cursor}}}<cursorPosition>
<contextAfterCursor>
{{{:context-after-cursor}}}")
    (defvar mg-minuet-gemini-fewshots
        `((:role "user"
           :content "# language: python
<contextBeforeCursor>
def fibonacci(n):
    <cursorPosition>
<contextAfterCursor>
fib(5)")
          ,(cadr minuet-default-fewshots)))
    (minuet-set-optional-options minuet-gemini-options
                                 :prompt 'mg-minuet-gemini-prompt
                                 :system)
    (minuet-set-optional-options minuet-gemini-options
                                 :template 'mg-minuet-gemini-chat-input-template
                                 :chat-input)
    (plist-put minuet-gemini-options :fewshots 'mg-minuet-gemini-fewshots)
    (minuet-set-optional-options minuet-gemini-options
                                 :generationConfig
                                 '(:maxOutputTokens 256
                                   :topP 0.9))
    (minuet-set-optional-options minuet-gemini-options
                                 :safetySettings
                                 [(:category "HARM_CATEGORY_DANGEROUS_CONTENT"
                                   :threshold "BLOCK_NONE")
                                  (:category "HARM_CATEGORY_HATE_SPEECH"
                                   :threshold "BLOCK_NONE")
                                  (:category "HARM_CATEGORY_HARASSMENT"
                                   :threshold "BLOCK_NONE")
                                  (:category "HARM_CATEGORY_SEXUALLY_EXPLICIT"
                                   :threshold "BLOCK_NONE")])
    )
(defun bjodah/minuet-use-groq-coder ()
  "Switch minuet provider to qwen2.5-Coder-32B via GROQ."
  (interactive)
  (setq minuet-n-completions 2)
  (setq minuet-context-window 2048)
  (plist-put minuet-openai-fim-compatible-options :end-point "https://api.groq.com/openai/v1/completions") ;; <--- GROQ only supports /chat/completions endpoint :/ (2025-03-31)
  (plist-put minuet-openai-fim-compatible-options :name "GROQ/Qwen/Qwen2.5-Coder-32B")
  (plist-put minuet-openai-fim-compatible-options :api-key (lambda () (getenv "GROQ_API_KEY")))
  (plist-put minuet-openai-fim-compatible-options :model "qwen-2.5-coder-32b")
  (minuet-set-optional-options minuet-openai-fim-compatible-options :suffix nil :template)
  (minuet-set-optional-options
   minuet-openai-fim-compatible-options
   :prompt
   (defun minuet-llama-cpp-fim-qwen-prompt-function (ctx)
     (format "<|fim_prefix|>%s\n%s<|fim_suffix|>%s<|fim_middle|>"
             (plist-get ctx :language-and-tab)
             (plist-get ctx :before-cursor)
             (plist-get ctx :after-cursor)))
   :template)
  (setq minuet-provider 'openai-fim-compatible)
  )

(defun bjodah/minuet-use-localhost-fim ()
    (setq minuet-n-completions 1) ; 1 is recommended for Local LLM for resource saving
    ;; I recommend beginning with a small context window size and incrementally
    ;; expanding it, depending on your local computing power. A context window
    ;; of 512, serves as an good starting point to estimate your computing
    ;; power. Once you have a reliable estimate of your local computing power,
    ;; you should adjust the context window to a larger value.
    (setq minuet-context-window 2048) ;; 512
    (plist-put minuet-openai-fim-compatible-options :end-point "http://localhost:8000/v1/completions")
    ;; an arbitrary non-null environment variable as placeholder
    (plist-put minuet-openai-fim-compatible-options :name "Tabby-localhost-8000")
    (plist-put minuet-openai-fim-compatible-options :api-key (defun my-tabby-api-key () "duck123"))
    ;; The model is set by the llama-cpp server and cannot be altered
    ;; post-launch.
    (plist-put minuet-openai-fim-compatible-options :model "PLACEHOLDER")

    ;; Llama.cpp does not support the `suffix` option in FIM completion.
    ;; Therefore, we must disable it and manually populate the special
    ;; tokens required for FIM completion.
    (minuet-set-optional-options minuet-openai-fim-compatible-options :suffix nil :template)
    (minuet-set-optional-options
     minuet-openai-fim-compatible-options
     :prompt
     (defun minuet-llama-cpp-fim-qwen-prompt-function (ctx)
         (format "<|fim_prefix|>%s\n%s<|fim_suffix|>%s<|fim_middle|>"
                 (plist-get ctx :language-and-tab)
                 (plist-get ctx :before-cursor)
                 (plist-get ctx :after-cursor)))
     :template)
    (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 256) ; or 56 for local llm?
    (setq minuet-provider 'openai-fim-compatible)
  )


(defun bjodah/minuet-use-smollm2 ()
    ;; GROQ for speed:
    (plist-put minuet-openai-compatible-options :end-point "http://localhost:8001/v1/chat/completions")
    (plist-put minuet-openai-compatible-options :api-key (defun my-tabby-api-key () "duck123"))
    ;(plist-put minuet-openai-compatible-options :model "HuggingFaceTB/SmolLM2-1.7B-Instruct")
    (plist-put minuet-openai-compatible-options :model "/my-llms/smollm2-1.7B-awq")

    ;; Prioritize throughput for faster completion
    (minuet-set-optional-options minuet-openai-compatible-options :max_tokens 128)
    (minuet-set-optional-options minuet-openai-compatible-options :top_p 0.9)
    (setq minuet-context-window 200) ;; TODO: we need to reset this to 16000 somewhere
    ;; TODO: customize the prompts, this is not a great model for coding, more conversational style.
    )

(use-package minuet
  :ensure t
  :bind
 (("M-o" . #'minuet-complete-with-minibuffer) ;; use minibuffer for completion
     ("M-i" . #'minuet-show-suggestion) ;; use overlay for completion
     ("M-p" . #'minuet-configure-provider)
     :map minuet-active-mode-map
     ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
     ("M-p" . #'minuet-previous-suggestion) ;; invoke completion or cycle to next completion
     ("M-n" . #'minuet-next-suggestion) ;; invoke completion or cycle to previous completion
     ("M-A" . #'minuet-accept-suggestion) ;; accept whole completion
     ;; Accept the first line of completion, or N lines with a numeric-prefix:
     ;; e.g. C-u 2 M-a will accepts 2 lines of completion.
     ("M-a" . #'minuet-accept-suggestion-line)
     ("M-e" . #'minuet-dismiss-suggestion))

    :init
    ;; if you want to enable auto suggestion.
    ;; Note that you can manually invoke completions without enable minuet-auto-suggestion-mode

    ;(add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)

    :config
    (bjodah/minuet-use-localhost-fim) ; or bjodah/minuet-use-smollm2
    (setq minuet-auto-suggestion-throttle-delay 0.5)
    (setq minuet-auto-suggestion-debounce-delay 0.3)

    ;; You can use M-x minuet-configure-provider to interactively configure provider and model
    )
