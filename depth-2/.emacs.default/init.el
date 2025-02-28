(if (and (getenv "DISPLAY") (string-match-p "dark" (shell-command-to-string
                             "gsettings get org.gnome.desktop.interface gtk-theme")))
                                        ;(set-background-color "black")
    (load-theme 'tango-dark)
    )
;(set-background-color "black")
(setq custom-file (concat user-emacs-directory "custom-vars.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; (if (boundp 'native-comp-eln-load-path)
;;     (setq native-comp-eln-load-path (expand-file-name (format "eln-cache-%d/" emacs-major-version) user-emacs-directory))
;; )
(setq visible-bell nil
      ring-bell-function 'flash-mode-line)
(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

(if (string-match-p "^Linux" (shell-command-to-string "uname"))
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 110 :width normal)))))
  (message "on MS Windows?")
  )

;; https://emacs-lsp.github.io/lsp-mode/page/performance/ ----------------------------------------
(setq gc-cons-threshold (* 128 1024 1024))
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; -----------------------------------------------------------------------------------------------

(defun bjodah/customize-frame (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil)))
  )

(defun bjodah/customize-window ()
  (cond
   ((string= system-name "argus") (set-face-attribute 'default nil :height 140))
   (t (set-face-attribute 'default nil :height 105))
   )
  (scroll-bar-mode 0)
      (global-unset-key (kbd "C-z"))     ;; (suspend-frame)
      (set-frame-font "Fira Code"))

(add-hook 'after-init-hook 'bjodah/customize-window)
(if (daemonp)
    (add-hook 'server-after-make-frame-hook
              (lambda ()
                (select-frame (car (frame-list)))
                (bjodah/customize-window)))
  )
(unless window-system
  (global-set-key (kbd "<mouse-4>") (lambda () (interactive) (scroll-down-line 4)))
  (global-set-key (kbd "<mouse-5>") (lambda () (interactive) (scroll-up-line 4)))
  (xterm-mouse-mode))

(bjodah/customize-window)

(require 'savehist)
(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))

(require 'recentf)
(recentf-mode 1)


(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)
(package-initialize)

;; Download use-package
(unless (package-installed-p 'use-package)
  (progn
    (package-refresh-contents)
    (package-install 'use-package)))

;http://www.emacswiki.org/emacs/LoadPath
(add-to-list 'load-path
                                        ;"~/.emacs.d/lisp/"
             (format "%s%s" (file-name-directory load-file-name) "lisp/")
             ;(format "%s%s" (file-name-directory load-file-name) "lisp/whisper.el/")
             )
;(load-file (format "%s%s" (file-name-directory load-file-name) "lisp/whisper.el/whisper.el"))
(require 'whisper)
(require 'my-text-to-speech)

;; whisper for Speech-To-Text (STT)
(use-package whisper
  ;:load-path (format "%s%s" (file-name-directory load-file-name) "lisp/whisper.el/")
  ;:ensure
  :bind ("C-c ," . whisper-run)
  :config
  (setq whisper-install-whispercpp nil ;'manual
        ;whisper-install-directory "/opt/"
        whisper-server-mode 'custom
        whisper-server-host "127.0.0.1"
        whisper-server-port 8007 ;8642
        whisper-model "large-v3-turbo"
        whisper-language "en" ;; "sv"
        whisper-translate nil
        ;whisper-use-threads (/ (num-processors) 2)
        ))

(use-package gptel
  :ensure t
  :config
  (setq
   gptel-model 'gemini-2.0-flash-exp
   gptel-backend (gptel-make-gemini "Gemini"
                                    :key (lambda () (shell-command-to-string "cat ~/doc/it/*nycklar*/g-gmni.* | tail -c+19 | head -c 39"))
                                    :stream t))
  :bind ("C-c ." . 'gptel-send)

                                        ;(setq gptel-api-key "your key")
  )


(gptel-make-openai "Groq"
  :host "api.groq.com"
  :endpoint "/openai/v1/chat/completions"
  :stream t
  :key (lambda () (shell-command-to-string "cat ~/doc/it/*nycklar*/grq-min-nyckel-16feb.txt | tail -c+19 | head -c -6"))

  :models '(deepseek-r1-distill-llama-70b-specdec
            llama-3.3-70b-specdec
            llama-3.3-70b-versatile
            llama-3.1-8b-instant
            llama-3.2-3b-preview
            qwen-2.5-coder-32b
            gemma2-9b-it))

;; xAI offers an OpenAI compatible API
(gptel-make-openai "xAI"           ;Any name you want
  :host "api.x.ai"
  :key (lambda () (shell-command-to-string "cat ~/doc/it/*nycklar*/xai-2025-feb.*"))
  :endpoint "/v1/chat/completions"
  :stream t
  :models '(;; xAI now only offers `grok-beta` as of the time of this writing
            grok-beta))

(gptel-make-openai "localhost-8000"
  :stream t
  :protocol "http"
  :host "localhost:8000"
  :key "duck123" ;(lambda () (shell-command-to-string "cat ~/doc/it/*nycklar*/vllm-local-api-key.txt"))
  ;:models '(Qwen/Qwen2.5-Coder-32B-Instruct-AWQ)
  ;:models '(stelterlab/phi-4-AWQ)
  :models '(TabbyAPI-QwenCoder14B)
)

(use-package minuet
  :ensure t
  :bind
 (("M-y" . #'minuet-complete-with-minibuffer) ;; use minibuffer for completion
     ("M-i" . #'minuet-show-suggestion) ;; use overlay for completion
     ("C-c m" . #'minuet-configure-provider)
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
    ;; You can use M-x minuet-configure-provider to interactively configure provider and model
    (setq minuet-provider 'openai-fim-compatible)
    (setq minuet-n-completions 3) ; recommended for Local LLM for resource saving
    ;; I recommend beginning with a small context window size and incrementally
    ;; expanding it, depending on your local computing power. A context window
    ;; of 512, serves as an good starting point to estimate your computing
    ;; power. Once you have a reliable estimate of your local computing power,
    ;; you should adjust the context window to a larger value.
    (setq minuet-context-window 512)
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

    (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 56))


;; Reduce load time
(eval-when-compile (require 'use-package))

(if (string-match-p "^Linux" (shell-command-to-string "uname"))
    (progn 

      (use-package vterm
        :ensure t
        :config
        (define-key vterm-mode-map (kbd "<f1>") nil)
        (define-key vterm-mode-map (kbd "<f2>") nil))

      (use-package rg
        :ensure t
        :ensure-system-package
        (rg . ripgrep)
        :config
        (global-set-key (kbd "M-s g") 'rg)
        (global-set-key (kbd "M-s d") 'rg-dwim))

      ;; (require 'dap-gdb-lldb)
      ;; M-x dap-gdb-lldb-setup
      (use-package dap-mode
        ;;  :ensure t)
        :ensure t
        :defer t
        :custom
        (dap-auto-configure-mode t                           "Automatically configure dap.")
        (dap-auto-configure-features
         '(sessions locals breakpoints expressions tooltip)  "Remove the button panel in the top.")
        :bind (("<f5>" . dap-step-in)
               ("<f6>" . dap-next)
               ("<f7>" . dap-step-out)
               ("<f8>" . dap-continue)
               ("<f9>" . dap-breakpoint-toggle))

        :config
        (require 'dap-lldb)
        (setq dap-lldb-debug-program '("/usr/bin/lldb-vscode-11"))
        (setq dap-lldb-debugged-program-function (lambda () (read-file-name "Select file to debug.")))        
  ;;; default debug template for (c++)
        (dap-register-debug-template
         "C++ LLDB dap"
         (list :type "lldb-vscode"
               :cwd nil
               :args nil
               :request "launch"
               :program nil))
        
        (defun dap-debug-create-or-edit-json-template ()
          "Edit the C++ debugging configuration or create + edit if none exists yet."
          (interactive)
          (let ((filename (concat (lsp-workspace-root) "/launch.json"))
	        (default "~/.emacs.d/default-launch.json"))
            (unless (file-exists-p filename)
	      (copy-file default filename))
            (find-file-existing filename)))
      ;;; Python
        (require 'dap-python)
        (setq dap-python-debugger 'debugpy)
        (dap-register-debug-template "Python :: Run pytest (at point)"
                                     (list :type "python-test-at-point"
                                           :args ""
                                           :module "pytest"
                                           :request "launch"
                                           :debugger 'debugpy
                                           :name "Python :: Run pytest (at point)"))        
        )
))

;; (use-package evil
;;   :ensure t ;; install the evil package if not installed
;;   :init ;; tweak evil's configuration before loading it
;;   (setq evil-search-module 'evil-search)
;;   (setq evil-ex-complete-emacs-commands nil)
;;   (setq evil-vsplit-window-right t)
;;   (setq evil-split-window-below t)
;;   (setq evil-shift-round nil)
;;   (setq evil-want-C-u-scroll t)
;;   :config ;; tweak evil after loading it
;;   (evil-mode)

;;   ;; example how to map a command in normal mode (called 'normal state' in evil)
;;   (define-key evil-normal-state-map (kbd ", w") 'evil-window-vsplit))

(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode))
(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (add-to-list 'company-backends 'company-capf)
  (add-to-list 'company-backends 'company-files)
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 1)
  :bind ("C-," . 'company-files)
  )

;; treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-file-follow-delay             0.2
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

;; parts from jamescherti/minimal-emacs.d below:
(use-package vertico ;; e.g. M-x now shows multiple choices
  :ensure t
  :defer t
  :commands vertico-mode
  :hook (after-init . vertico-mode))

(use-package orderless
  ;; Vertico leverages Orderless' flexible matching capabilities, allowing users
  ;; to input multiple patterns separated by spaces, which Orderless then
  ;; matches in any order against the candidates.
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia  ;; helpful text to the right of vertico options
  ;; Marginalia allows Embark to offer you preconfigured actions in more contexts.
  ;; In addition to that, Marginalia also enhances Vertico by adding rich
  ;; annotations to the completion candidates displayed in Vertico's interface.
  :ensure t
  :defer t
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode))

(use-package embark
  ;; Embark is an Emacs package that acts like a context menu, allowing
  ;; users to perform context-sensitive actions on selected items
  ;; directly from the completion interface.
  :ensure t
  :defer t
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  ;; Enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

(use-package corfu
  :ensure t
  :defer t
  :commands (corfu-mode global-corfu-mode)

  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  :custom
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Disable Ispell completion function. As an alternative try `cape-dict'.
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)

  ;; Enable Corfu
  :config
  (global-corfu-mode))

(use-package cape
  :ensure t
  :defer t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))



(use-package sqlite3
  :ensure t)
(use-package magit
  :defer t
  :ensure forge)
(use-package forge
  :after magit)
(use-package treemacs-magit
  :ensure t
  :after treemacs magit)

;; lsp
(use-package lsp-mode
  :ensure t
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l")
  :custom
  (lsp-file-watch-threshold 4000)
  ;; (lsp-rust-server 'rls)
  ;; (lsp-rust-rls-server-command "/opt/cargo/bin/rls")
  :hook (
         (c-mode . lsp)
         (c++-mode . lsp)
         (rust-mode . lsp)
         ;; (sh-mode . lsp)  ;; bash-ls is such a joke...
         (typescript-mode . lsp)
         (python-mode . lsp)
         (java-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
)



;; (use-package ccls ;; I'm using clangd for now, might be interesting for parallel emacs conf.
;;   :ensure t
;;   :hook ((c-mode c++-mode) .
;;          (lambda () (require 'ccls) (lsp)))
;;   :config
;;   (with-eval-after-load "lsp-mode"
;;     (add-to-list 'lsp-enabled-clients 'ccls))
;; )
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (with-eval-after-load "lsp-mode"
    (setq lsp-ui-doc-show-with-cursor nil) ;; keep on-mouse-over docs
    (setq lsp-ui-sideline-enable nil) ;; e.g. "Extract expression into function...."
    (setq lsp-lens-enable nil) ;; bullet 3: https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
    )
  :bind
  ("C-<down>" . lsp-ui-find-next-reference)
  ("C-<up>" . lsp-ui-find-prev-reference)
  )

(use-package lsp-pyright
  :ensure t
  :init (setq lsp-pyright-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(use-package lsp-java
  :ensure t
  ;; :after lsp
  :config
  ;; (with-eval-after-load "lsp-mode"
  ;;   (add-to-list 'lsp-enabled-clients 'jdtls))
  (setq lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
        lsp-java-format-settings-profile "GoogleStyle"
        lsp-java-save-actions-organize-imports t
        lsp-java-references-code-lens-enabled t
        lsp-java-implementations-code-lens-enabled t
        lsp-file-watch-ignored
        '(".idea" ".ensime_cache" ".eunit" "node_modules"
          ".git" ".hg" ".fslckout" "_FOSSIL_"
          ".bzr" "_darcs" ".tox" ".svn" ".stack-work"
          "build"))
)

;; (use-package company-lsp <--- deprecated apparently company-capf is used instead
;;   :ensure t
;;   :commands company-lsp)

;; (use-package lsp-jedi <--- I found jedi to be somewhat buggy, going to try ms instead
;;   :ensure t
;;   :config
;;   (with-eval-after-load "lsp-mode"
;;     (add-to-list 'lsp-disabled-clients 'pyls)
;;     ;; (add-to-list 'lsp-enabled-clients 'jedi)
;;     ))

;; (use-package jedi
;;   :ensure t
;;   :bind ("C-<tab>" . jedi:complete)
;;   :init
;;   (add-hook 'python-mode-hook 'jedi:setup)
;;   (setq jedi:complete-on-dot t))

;; (setq jedi:setup-keys t) ;; <--- Lets Jedi set keys


;; (setq lsp-json-schemas `[(:fileMatch ["tsconfig.json"] :url "http://json.schemastore.org/tsconfig")])


(use-package rust-mode
  :ensure t
  ;;   :config
  ;; (with-eval-after-load "lsp-mode"
  ;;   (add-to-list 'lsp-enabled-clients 'rls))
  )

(use-package typescript-mode
  :ensure t
  ;;   :config
  ;; (with-eval-after-load "lsp-mode"
  ;;   (add-to-list 'lsp-enabled-clients 'ts-ls))
  )

;; (use-package flycheck-rust
;; 	     :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; (use-package dap-lldb
;;   :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))
(setq ccls-executable "/usr/local/bin/ccls")


(use-package cython-mode :ensure t)
(add-hook 'cython-mode-hook (lambda () (which-function-mode -1))) ;; https://github.com/bbatsov/prelude/issues/940#issuecomment-210505475
(use-package dockerfile-mode :ensure t)
(use-package realgud :ensure t)

;; (use-package quelpa-use-package
;;   :init (setq quelpa-update-melpa-p nil)
;;   :config (quelpa-use-package-activate-advice))


(use-package ein
  :ensure t
  :bind
  ("C-c 1" . ein:worksheet-execute-all-cells)
  ("C-c 2" . ein:worksheet-execute-all-cells-above)
  ("C-c 3" . ein:worksheet-execute-all-cells-below)
  ("C-c 9" . ein:worksheet-clear-all-output)
  ("C-c 0" . ein:notebook-restart-session-command)
  :config
  (setq ein:worksheet-enable-undo t
        ein:output-area-inlined-images t)
  )

(use-package god-mode
  :ensure t
  :bind
  ("C-<escape>" . #'god-local-mode)
  ("ESC M-SPC" . #'god-local-mode)
  :config
  (defun my-god-mode-update-cursor-type ()
    (if god-local-mode
        (progn
          (setq cursor-type 'hbar)
          (setq blink-cursor-interval 0.15)
          (setq blink-cursor-blinks 30)
      )
      (progn
          (setq cursor-type 'box)
          (setq blink-cursor-interval 0.5)
          (setq blink-cursor-blinks 10)
        )))
  (add-hook 'post-command-hook #'my-god-mode-update-cursor-type)
)

(use-package jupyter
  :ensure t)
(use-package tex
  :defer t
  ;:ensure auctex
  :config
  (setq TeX-auto-save t))
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install t))

(use-package cmake-mode
  :ensure t)
(use-package yaml-mode
  :ensure t)
(use-package mmm-mode
  :ensure t)
;; mmm-mako


(message (format "%s%s" (file-name-directory load-file-name) "lisp/"))
(require 'sln-mode)
(require 'mmm-mako)
;; (require 'mmm-mode)
;; (load-file "~/.emacs.d/lisp/mmm-mako.el")
;; - Makefile
(add-to-list 'auto-mode-alist '("\\.mk.mako\\'" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.mk.mako\\'" . mmm-mode))
(mmm-add-mode-ext-class 'makefile-gmake-mode "\\.mk.mako\\'" 'mako)
;; - C++
(add-to-list 'auto-mode-alist '("\\.cpp.mako\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp.mako\\'" . mmm-mode))
(mmm-add-mode-ext-class 'c++-mode "\\.cpp.mako\\'" 'mako)

(add-to-list 'auto-mode-alist '("\\.hpp.mako\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp.mako\\'" . mmm-mode))
(mmm-add-mode-ext-class 'c++-mode "\\.hpp.mako\\'" 'mako)

(add-to-list 'auto-mode-alist '("\\.ipp.mako\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ipp.mako\\'" . mmm-mode))
(mmm-add-mode-ext-class 'c++-mode "\\.ipp.mako\\'" 'mako)
;; - C
(add-to-list 'auto-mode-alist '("\\.c.mako\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.c.mako\\'" . mmm-mode))
(mmm-add-mode-ext-class 'c-mode "\\.c.mako\\'" 'mako)

(add-to-list 'auto-mode-alist '("\\.h.mako\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.h.mako\\'" . mmm-mode))
(mmm-add-mode-ext-class 'c-mode "\\.h.mako\\'" 'mako)

;(use-package monokai-theme :ensure t)
;(use-package tangotango-theme :ensure t)
(require 'pitchkai-theme)


;; yasnippet
(setq yas-indent-line 'fixed)
;; This is the proper way to rebind yasnippet key
;; (see https://github.com/capitaomorte/yasnippet/issues/296)
; (define-key yas-minor-mode-map (kbd "C-@") 'yas/expand)
; (define-key yas-minor-mode-map (kbd "TAB") nil)
; (define-key yas-minor-mode-map (kbd "TAB") 'yas/expand)
; (define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand) 
; ;next-field-or-maybe-expand <-- mark with space alt?.
;(define-key yas-minor-mode-map (kbd "TAB") nil)
(use-package validate
  :ensure t)
(require 'validate)

(use-package yasnippet
  :ensure t
  :bind
  ("C-c y s" . yas-insert-snippet)
  ("C-c y v" . yas-visit-snippet-file)
  :config
  (validate-setq
   ;; yas-verbosity 1
   yas-wrap-around-region t)
  ;; (with-eval-after-load 'yasnippet
  ;;   (validate-setq yas-snippet-dirs '(yasnippet-snippets-dir)))
  ;; (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  ;; (yas-reload-all)
  )

(use-package yasnippet-snippets
  :ensure t
  :defer t
  )

(use-package elfeed
  :ensure t
  :config
  (global-set-key (kbd "C-x w") 'elfeed)
  (setq elfeed-feeds
        '("https://www.phoronix.com/rss.php"
          "https://fa.bianp.net/blog/feed/"
          "https://lemire.me/blog/feed/"
          "https://hbfs.wordpress.com/feed/"
          "https://easyperf.net/feed.xml"
          "https://pvk.ca/atom.xml"
          "https://stackoverflow.com/feeds/tag?tagnames=x86&sort=votes"
          "https://branchfree.org/feed/"
          "https://stackoverflow.com/feeds/user/2542702" ;; Z boson
          ;;"https://stackoverflow.com/feeds/user/224132" ;; Peter Cordes
          "https://sachachua.com/blog/feed/" ;; Sacha Chua
          "https://pzemtsov.github.io/feed.xml"
          "https://gms.tf/feeds/all.atom.xml" ;; Georg Sauthoff
          "https://gpfault.net/rss.xml"
          "https://nhigham.com/feed/" ;; Nick Higham, The University of Manchester
          "https://depth-first.com/articles.atom" ;; Richard L. Apodaca (chemoinformatics)
          ))
  )

;(package-vc-install '(org-mode :url "https://code.tecosaur.net/tec/org-mode"))

(use-package org
  ;:ensure t // built-in I believe

  ;; :defer ;; https://abode.karthinks.com/org-latex-preview/
  ;; :ensure `(org :repo "https://code.tecosaur.net/tec/org-mode.git/"
  ;;               :branch "dev")

  :config
  (setq org-html-htmlize-output-type 'css) ; default: 'inline-css
  (setq org-html-htmlize-font-prefix "org-") ; default: "org-"
  

  ;; org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   (mapcar (lambda (lang) (cons lang t))
           `(C
             dot
             emacs-lisp
             ;julia
             python
             jupyter
             ,(if (locate-library "ob-shell") 'shell 'sh)
             )))
  (setq org-confirm-babel-evaluate nil)

  ;; Render mako from org-mode source block (https://stackoverflow.com/a/10418779/790973)
  (defun org-babel-execute:mako (body params)
    "Render Mako templated source with org-babel."
    (message "calling render-mako on code block")
    (org-babel-eval "mako-render" body))
  (setq org-babel-python-command "python3")

)

(setq python-shell-interpreter "python3")

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/doc/org-roam"))
  :bind (("C-c n l" . org-roam-buffer-toogle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol)
)

(use-package julia-mode
  :ensure t)

(use-package rmsbolt ;; live disassembly
  :ensure t)

(use-package nginx-mode
  :ensure t
  )

(use-package all-the-icons
  :ensure t
  ; :commands all-the-icons-install-fonts
  )

(use-package nerd-icons
  :ensure t
  ; :commands nerd-icons-install-fonts
  )

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package monokai-theme
  :ensure t
)


(use-package flymake-shellcheck
  :ensure t
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

(add-hook 'sh-mode-hook
          (function (lambda()
                      (add-to-list 'lsp-disabled-clients 'bash-ls))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global hooks and keymaps
(global-unset-key (kbd "C-x C-z")) ;; (suspend-frame)



(if (>= emacs-major-version 28)
    ;; (add-hook 'after-init-hook (lambda () (load-theme
    ;;                                     ;'tangotango
    ;;                                     ;'pitchkai
    ;;                                     ;'modus-vivendi
    ;;                                        'monokai
    ;;                                        )))
  (if (functionp 'tool-bar-mode)
    (tool-bar-mode 0))
)


(add-hook 'gdb-mode-hook
    (function (lambda ()
		(define-key gud-minor-mode-map (kbd "<f4>") #'gud-print)
		(define-key gud-minor-mode-map (kbd "<f5>") #'gud-step)
		(define-key gud-minor-mode-map (kbd "C-<f5>") #'gud-stepi)
		(define-key gud-minor-mode-map (kbd "<f6>") #'gud-next)
		(define-key gud-minor-mode-map (kbd "<f7>") #'gud-finish)
		(define-key gud-minor-mode-map (kbd "<f8>") #'gud-cont)
		(define-key gud-minor-mode-map (kbd "<f9>") #'gud-break)
		(define-key gud-minor-mode-map (kbd "C-<f9>") #'gud-tbreak)
		(define-key gud-minor-mode-map (kbd "<f10>") #'gud-until)
		(define-key gud-minor-mode-map (kbd "M-<f10>") #'gud-jump)
		(define-key gud-minor-mode-map (kbd "<f11>") #'gud-run)
		(define-key gud-minor-mode-map (kbd "M-<f11>") #'gud-kill)
		;(define-key gud-minor-mode-map (kbd "<f12>") #'gdb-many-windows)
                (define-key gud-minor-mode-map (kbd "<f12>") 
                  (lambda() (interactive) (progn (gdb-many-windows) (other-window) (other-window))))
		(define-key gud-minor-mode-map (kbd "M-<up>") #'gud-up)
		(define-key gud-minor-mode-map (kbd "M-<down>") #'gud-down)
		(define-key gud-minor-mode-map (kbd "<prior>") #'gud-up)
		(define-key gud-minor-mode-map (kbd "<next>") #'gud-down)
)))


(add-hook 'c++-mode-hook
    (function (lambda ()
        (yas-global-mode t)
	(electric-pair-mode)
        (setq c-basic-offset 4)
        (which-function-mode t)
        (add-hook 'write-contents-functions
                  (lambda()
                    (save-excursion
                      (delete-trailing-whitespace)
                      )))
        ;; (setq c-offsets-alist ((inline-open . 0)  ; custom indentation rules
        ;;                        (brace-list-open . 0)
        ;;                        (statement-case-open . +)))
        )))


(add-hook 'cmake-mode-hook
    (function (lambda ()
        (setq cmake-tab-width 4)
        )))

(make-variable-buffer-local 'compile-command)



;; yas-next-field-or-maybe-expand


;; (require 'smart-operator)
;; (add-hook 'python-mode-hook
;; 	  (function (lambda()
;; 		      (py-smart-operator-mode-p-on))))


;; Now for elpy - I can't quite make it play nice
;; - not going to look into this more now
;; (package-initialize)
;; (elpy-enable)
;; (elpy-use-ipython)


;; newline-withoug-break-of-line ;; http://stackoverflow.com/questions/5898448
(defun newline-without-break-of-line ()
"1. move to end of the line.
 2. insert indented newline"

  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x v e") 'vc-git-grep)
(global-set-key (kbd "<C-return>") 'newline-without-break-of-line)
;(global-set-key (kbd "C-c C-l") 'compile)
(global-set-key "\C-cb" 'insert-buffer-name)
(global-set-key (kbd "C-c m") 'recompile)
(global-set-key (kbd "C-x f") 'find-file-at-point)
(global-set-key (kbd "C-c M-f") 'set-fill-column)
(global-set-key (kbd "C-c C-l") 'hl-line-mode)
(global-set-key (kbd "<f2>") 'other-window)
(defun other-window-backward (&optional n)
  "like C-x o, but in the other direction"
  (interactive "p")
  (if n
      (other-window (- n))
  (other-window -1)))
(global-set-key (kbd "<f1>") 'other-window-backward)
(global-set-key "\C-xp" 'other-window-backward)
(global-set-key (kbd "C-<f1>") 'previous-buffer)
(global-set-key (kbd "C-<f2>") 'next-buffer)
(global-set-key (kbd "ESC <f1>") 'delete-other-windows)
(global-set-key (kbd "ESC <f2>") 'split-window-below)
(global-set-key (kbd "ESC <f3>") 'split-window-right)
(global-set-key (kbd "ESC <f4>") 'delete-window)
(global-set-key (kbd "C-c C-<left>") 'previous-buffer)
(global-set-key (kbd "C-c C-<right>") 'next-buffer)



(defun insert-buffer-name () (interactive)
  (insert (buffer-name))
)
; Let F3 insert current file name when in minibuffer
(define-key minibuffer-local-map [f3]
  (lambda() (interactive) (insert (buffer-file-name (nth 1 (buffer-list))))))


;; auto-complete (init after yasnippet)
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
;;(require 'auto-complete-config)
;(setq ac-auto-start 4) ;; need 4 characters before suggesting auto-completion

;; http://stackoverflow.com/questions/8674912/how-to-collapse-whitespaces-in-a-region
(defun just-one-space-in-region (beg end)
  "replace all whitespace in the region with single spaces"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "\\s-+" nil t)
        (replace-match " ")))))


(defun my/vterm-execute-region-or-current-line ()
  "Insert text of current line in vterm and execute."
  (interactive)
  (require 'vterm)
  (eval-when-compile (require 'subr-x))
  (let ((command (if (region-active-p)
                     (string-trim (buffer-substring
                                   (save-excursion (region-beginning))
                                   (save-excursion (region-end))))
                   (string-trim (buffer-substring (save-excursion
                                                    (beginning-of-line)
                                                    (point))
                                                  (save-excursion
                                                    (end-of-line)
                                                    (point)))))))
    (let ((buf (current-buffer)))
      (unless (get-buffer vterm-buffer-name)
        (vterm))
      (display-buffer vterm-buffer-name t)
      (switch-to-buffer-other-window vterm-buffer-name)
      (vterm--goto-line -1)
      (message command)
      (vterm-send-string command)
      (vterm-send-return)
      (switch-to-buffer-other-window buf)
      )))

(global-set-key (kbd "C-c z") 'my/vterm-execute-region-or-current-line)

;; 4 spaces indentation level for C/C++
(setq-default c-default-style "bsd")
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)


(add-hook 'ReST-mode-hook
    (function (lambda ()
       (flyspell-mode)
       (add-hook 'write-contents-functions
		 (lambda()
		   (save-excursion
		     (delete-trailing-whitespace)
                     )))
       )))


;; BEGIN LATEX
(add-hook 'LaTeX-mode-hook
    (function (lambda ()
       (reftex-mode)
       (flyspell-mode)
       (auto-fill-mode)
       (define-key LaTeX-mode-map "\C-c\C-t\C-x" 'TeX-toggle-escape)
       (setq fill-column 199))))


;; BEGIN AUCTEX MINTED FIX
; http://old.nabble.com/shell-escape-td1076639.html
(defun TeX-toggle-escape nil (interactive)
  (setq LaTeX-command
        (if (string= LaTeX-command "latex") "latex -shell-escape" "latex")))

; source: http://stackoverflow.com/questions/3300497/using-minted-source-code-latex-package-with-emacs-auctex?lq=1
;; (eval-after-load "tex"
;;   '(setcdr (assoc "LaTeX" TeX-command-list)
;;           '("%`%l%(mode) -shell-escape%' %t"
;;           TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX")
;;     )
;;   )
;; END AUCTEX MINTED FIX


;; === Spelling (grammar) ===
;; * TODO - start using wcheck mode instead of flyspell-mode
;; * Use hunspell (sudo apt-get install hunspell) for spell-checking
;; * http://stackoverflow.com/questions/3961119/working-setup-for-hunspell-in-emacs
(setq ispell-dictionary-alist
  '((nil "[A-Za-z]" "[^A-Za-z]" "[']" t ("-d" "en_US") nil utf-8)))

(if (file-exists-p "/usr/bin/hunspell")                                         
    (progn
      (setq ispell-program-name "hunspell")
      (eval-after-load "ispell"
        '(progn (defun ispell-get-coding-system () 'utf-8)))))

;; (setq ispell-local-dictionary-alist nil)
;; (add-to-list 'ispell-local-dictionary-alist 
;; 	     '("en_US" "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-d" "en_US") nil utf-8)
;;  	     )


;; (define-abbrev-table 'global-abbrev-table '(
;;     ("alphaQ" "α" nil 0)
;;     ("betaQ" "β" nil 0)
;;     ("gammaQ" "γ" nil 0)
;;     ("deltaQ" "δ" nil 0)
;;     ("DeltaQ" "Δ" nil 0)
;;     ("thetaQ" "θ" nil 0)
;;     ("muQ" "μ" nil 0)
;;     ("piQ" "π" nil 0)
;;     ("infQ" "∞" nil 0)
;;     ("ddagerQ" "‡" nil 0)
;;     ("ar1Q" "→" nil 0)
;;     ("ar2Q" "⇒" nil 0)
;;     ("dnmQ" "DO-NOT-MERGE!" nil 0)
;;     ))


;; http://emacswiki.org/emacs/BackupDirectory
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups


;(require 'python-mode)
;; Set minor modes for Python
(add-hook 'python-mode-hook
    (function (lambda ()
       (yas-global-mode t) ;; <-- I can't get minor mode to work properly
       (column-number-mode)
       ;(auto-complete-mode)
       (flyspell-prog-mode)
       ;(highlight-indentation-mode)
       (electric-pair-mode)
       (add-hook 'write-contents-functions
		 (lambda()
		   (save-excursion
		     (delete-trailing-whitespace)
                     ;(pep8)
                     )))
       ;(local-set-key (kbd "C-c C-c") 'py-execute-buffer-python3)
       (local-set-key (kbd "C-c o") 'pep8)
       (local-set-key (kbd "C-c p") (lambda () (interactive) (occur "\\bdef \\|\\bclass \\|=[ ]?lambda")))
       ;; (jedi:setup)
       )))
;; (fset 'pytoc
;;    [?\M-x ?o ?c ?c ?u ?r return ?d ?e ?f ?\\ ?b ?\\ ?| ?c ?l ?a ?s ?s ?\\ ?b ?\\ ?| ?= ?\[ ?  ?\] ?? ?l ?a ?m ?b ?d ?a return ?\C-x])


(defun ipython ()
    (interactive)
    (term "ipython")) ;; note: C-x becomes C-c in term

(add-hook 'markdown-mode-hook
    (function (lambda ()
        (flyspell-mode)
        (auto-fill-mode)
        )))

(add-to-list 'auto-mode-alist '("\\.tex$" . LaTeX-mode))
(add-to-list 'auto-mode-alist '("\\.ipp$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.pyx$" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("Containerfile" . dockerfile-mode))

; pylint pep8
;; (require 'tramp)
;; (require 'python-pep8)
;; (require 'python-pylint)

; deleting trailing space
;(add-hook 'before-save-hook 'delete-trailing-whitespace)


(eval-after-load 'ox '(require 'ox-koma-letter))


;; XPPAUT (xpp.el) xpp - mode
(autoload 'xpp-mode "xpp" "Enter XPP mode." t)
(setq auto-mode-alist (cons '("\\.ode\\'" . xpp-mode) auto-mode-alist))


(put 'upcase-region 'disabled nil)

;; (require 'package)
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; (require 'ycmd)
;; (add-hook 'c++-mode-hook 'ycmd-mode)

;; (require 'company-ycmd)
;; (company-ycmd-setup)

;; (set-variable 'ycmd-server-command '("python" "/home/bjorn/rovc/ycmd/ycmd"))

(put 'downcase-region 'disabled nil)


;; (require 'use-package)
;; (use-package company-rtags
;;   :after company)

;; (use-package rtags
;;   :commands rtags-mode
;;   :bind (("C-. r D" . rtags-dependency-tree)
;;          ("C-. r F" . rtags-fixit)
;;          ("C-. r R" . rtags-rename-symbol)
;;          ("C-. r T" . rtags-tagslist)
;;          ("C-. r d" . rtags-create-doxygen-comment)
;;          ("C-. r c" . rtags-display-summary)
;;          ("C-. r e" . rtags-print-enum-value-at-point)
;;          ("C-. r f" . rtags-find-file)
;;          ("C-. r i" . rtags-include-file)
;;          ("C-. r i" . rtags-symbol-info)
;;          ("C-. r m" . rtags-imenu)
;;          ("C-. r n" . rtags-next-match)
;;          ("C-. r p" . rtags-previous-match)
;;          ("C-. r r" . rtags-find-references)
;;          ("C-. r s" . rtags-find-symbol)
;;          ("C-. r v" . rtags-find-virtuals-at-point))
;;   :bind (:map c-mode-base-map
;;               ("M-." . rtags-find-symbol-at-point)))



;; https://stackoverflow.com/a/13408008/790973
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; https://stackoverflow.com/a/3592559/790973
;; (add-hook 'c-mode-common-hook 
;;           (lambda () (define-key c-mode-base-map (kbd "C-c C-l") 'compile)))

;; https://www.emacswiki.org/emacs/WinnerMode
(winner-mode 1)

(show-paren-mode 1)



;; (fset 'mark-to-space
;;    (kmacro-lambda-form [?\C-  ?\C-s ?  ?\C-b] 0 "%d"))
(fset 'mark-to-space
   (kmacro-lambda-form [?\C-  ?\M-x ?i ?s ?e tab ?- ?f ?o ?r tab ?- ?r ?e ?g tab return ?\\ ?s ?- ?\M-x ?i ?s tab ?r backspace ?e tab ?- ?b ?a ?c tab ?- ?r ?e tab return ?\\ ?w ?\C-f] 0 "%d"))
(global-set-key (kbd "C-c SPC") 'mark-to-space)


;; (fset 'comment-c-word
;;    (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([47 kp-multiply 32 134217848 134217840 134217840 return 91 44 41 93 return 2 32 42 47] 0 "%d")) arg)))
;; (global-set-key (kbd "C-c c") 'comment-c-word)

(let ((local-settings (concat user-emacs-directory "local-settings.el")))
 (when (file-exists-p local-settings)
   (load-file local-settings))
  ;; e.g.:
  ;; (defun open-main-org ()
  ;;   "Just open my personal master document"
  ;;   (interactive)
  ;;   (find-file "//ODEN/Profile\$/bjorningvar/Documents/main.org"))
  ;; (open-main-org)
)


;;; init.el ends here
