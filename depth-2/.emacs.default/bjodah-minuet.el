(defun bjodah/minuet-use-groq ()
    ;; GROQ for speed:
    (plist-put minuet-openai-compatible-options :end-point "https://api.groq.com/openai/v1/chat/completions")
    (plist-put minuet-openai-compatible-options :api-key (lambda () (shell-command-to-string "cat ~/doc/it/*nycklar*/grq-min-nyckel-16feb.txt | tail -c+19 | head -c -6")))
    (plist-put minuet-openai-compatible-options :model "llama-3.3-70b-specdec")

    ;; Prioritize throughput for faster completion
    ;(minuet-set-optional-options minuet-openai-compatible-options :provider '(:sort "throughput"))
    (minuet-set-optional-options minuet-openai-compatible-options :max_tokens 256)
    (minuet-set-optional-options minuet-openai-compatible-options :top_p 0.9)
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
    (setq minuet-auto-suggestion-throttle-delay 0.5)
    (setq minuet-auto-suggestion-debounce-delay 0.3)

    ;; You can use M-x minuet-configure-provider to interactively configure provider and model
    (setq minuet-provider 'openai-fim-compatible)  ;; change to 'OpenAI-compatible to use GROQ/Smollm2
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
    )
