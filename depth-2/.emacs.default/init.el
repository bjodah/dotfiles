;;; init.el --- Initialization file for Emacs  -*- lexical-binding: t -*-

;;; Commentary:
;; Emacs Startup File --- initialization for Emacs

;; Unset some default key-bindings.  I prefer C-x (, C-x ), C-x e for macro related functions
(global-unset-key (kbd "<f3>")) ; unbind kmacro-start-macro-or-insert-counter from F3
(global-unset-key (kbd "<f4>")) ; unbind kmacro-end-or-call-macro from F4
(global-unset-key (kbd "C-x C-z")) ;; (suspend-frame)

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


;; -----------------------------------------------------------------------------------------------

(defun bjodah/customize-frame (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil)))
  )
(defun bjodah/customize-window ()
  (cond
   ((string= (system-name) "argus")
    (set-face-attribute 'default nil :height 140))
   (t
    (set-face-attribute 'default nil :height 105)))
  (scroll-bar-mode 0)
      (global-unset-key (kbd "C-z"))     ;; (suspend-frame)
      (set-frame-font "Fira Code"))
(add-hook 'after-load-theme-hook 'bjodah/customize-window)

;; (defun bjodah/startup-theme ()
;;   (cond
;;    ((string= (system-name) "SE-BDAHLGREN2") (load-theme 'modus-operandi-tinted))
;;    (t (load-theme 'doom-monokai-ristretto t))))
;; (add-hook 'after-init-hook 'bjodah/startup-theme)

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

(defun bjodah/select-theme ()
  (cond
   ((string= (system-name) "SE-BDAHLGREN2") (load-theme 'modus-operandi-tinted))
   ((and (> (length (getenv "DISPLAY")) 0) (not (string= "'prefer-dark'" (string-trim (shell-command-to-string
                                  "gsettings get org.gnome.desktop.interface color-scheme"))))) (load-theme 'modus-operandi-tinted))
   (t (load-theme 'doom-monokai-ristretto t))))

(add-hook 'after-init-hook 'bjodah/select-theme)


;(bjodah/customize-window)

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

(add-to-list 'load-path ;http://www.emacswiki.org/emacs/LoadPath
             (format "%s%s" (file-name-directory load-file-name) "lisp/")
            ;(format "%s%s" (file-name-directory load-file-name) "lisp/whisper.el/")
             )

(use-package emacs
  :ensure nil
  :custom
  (kill-do-not-save-duplicates t)
  (add-hook 'text-mode-hook 'visual-line-mode)
  (require-theme 'modus-themes)
  (setq modus-themes-common-palette-overrides
        modus-themes-preset-overrides-intense)
  (add-hook 'modus-themes-after-load-theme-hook 'bjodah/customize-window)
  ;(enable-recursive-minibuffers t "Allow minibuffer commands in the minibuffer") ;see mb-depth
  :bind
  (:map global-map
        ("C-c M-! 1" . modus-themes-toggle)
        ("C-c M-T t" . customize-themes)
        ("C-c M-V" . visual-line-mode)
        ("C-c M-F" . auto-fill-mode)
        ("M-F"     . fill-region)
        ("M-K" . (lambda ()
                  (interactive)
                  (kill-buffer (current-buffer))))
        ))

;; (use-package mb-depth
;;   :config
;;   (minibuffer-depth-indicate-mode 1))

(use-package treesit
  :ensure nil ;; C-h v system-configuration-options, look for --with-tree-sitter)
  :config
  (setq treesit-font-lock-level 4))

(use-package autorevert
  :ensure nil
  :config
  (setq global-auto-revert-non-file-buffers t))

(use-package ispell
  :ensure nil
  :init
  (if (executable-find "hunspell")
      (setq ispell-program-name "hunspell"))
        
  :config
  (when (string= ispell-program-name "hunspell")
    (defun ispell-get-coding-system () 'utf-8)
    (setq ispell-dictionary "en_GB,bjodah")
    (ispell-set-spellchecker-params)
    (ispell-hunspell-add-multi-dic "en_GB,bjodah")))


;; (ispell-change-dictionary "en_GB" t)

;; (setq ispell-local-dictionary-alist nil)
;; (add-to-list 'ispell-local-dictionary-alist 
;; 	     '("en_US" "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-d" "en_US") nil utf-8)
;;  	     )
;)


;(load-file (format "%s%s" (file-name-directory load-file-name) "lisp/whisper.el/whisper.el"))
(require 'strisper)
(global-set-key (kbd "C-c M-R") 'strisper-record-at-point)

(require 'whisper)
(require 'my-text-to-speech)

;; whisper for Speech-To-Text (STT)
(use-package whisper
  ;:load-path (format "%s%s" (file-name-directory load-file-name) "lisp/whisper.el/")
  ;:ensure
  :bind (("C-c ," . whisper-run)
         ("<f4>" . whisper-run))
  :config
  (setq whisper-install-whispercpp nil ;'manual
        ;whisper-install-directory "/opt/"
        whisper-server-mode 'custom
        whisper-server-host "127.0.0.1"
        whisper-server-port 8007 ;8642  ; see "host-speeches-ai-8007.sh"
        whisper-model "large-v3-turbo"
        whisper-language "en" ;; "sv"
        whisper-translate nil
        ;whisper-use-threads (/ (num-processors) 2)
        ))

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
        :bind(:map dap-mode-map
                   ("<f5>" . dap-step-in)
                   ("<f6>" . dap-next)
                   ("<f7>" . dap-step-out)
                   ("<f8>" . dap-continue)
                   ("<f9>" . dap-breakpoint-toggle))
        ;;:bind
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
;; (use-package company
;;   :ensure t
;;   :hook (prog-mode . company-mode)
;;   :config
;;   (add-to-list 'company-backends 'company-capf)
;;   (add-to-list 'company-backends 'company-files)
;;   (setq company-tooltip-align-annotations t)
;;   (setq company-minimum-prefix-length 1)
;;   :bind ("C-," . 'company-files)
;;   )

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

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package embark
  ;; Embark is an Emacs package that acts like a context menu, allowing
  ;; users to perform context-sensitive actions on selected items
  ;; directly from the completion interface.
  :ensure t
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-c M-c" . embark-act)
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
         ("<f3>" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x P b" . consult-project-buffer)
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

(use-package dired-preview
  :ensure t
  :config
  (setq dired-preview-delay 0.025)
  (setq dired-kill-when-opening-new-dired-buffer t)
)

(use-package dired-filter
  :ensure t
                                        ;:bind(:map dired-mode-map ("/" . dired-filter-map))
  )

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

(load (expand-file-name (concat user-emacs-directory "bjodah-gptel")))
(load (expand-file-name (concat user-emacs-directory "bjodah-minuet")))
(load (expand-file-name (concat user-emacs-directory "bjodah-ahyatt-llm")))
(load (expand-file-name (concat user-emacs-directory "bjodah-lsp")))
(load (expand-file-name (concat user-emacs-directory "bjodah-tools")))
(load (expand-file-name (concat user-emacs-directory "bjodah-latex")))

(global-set-key (kbd "M-Z") 'bjodah/mark-to-char-before-literal)
(global-set-key (kbd "C-c Z") 'bjodah/vterm-execute-region-or-current-line)
(global-set-key (kbd "C-c T") 'bjodah/transpose1)


(use-package aidermacs
  :ensure t
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  :custom
  ; See the Configuration section below
  (aidermacs-use-architect-mode t)
  ;; (aidermacs-architect-model "openrouter/deepseek/deepseek-r1")
  ;; (aidermacs-default-model "openrouter/deepseek/deepseek-chat")
  (aidermacs-architect-model "litellm_proxy/local-qwq-32b")
  (aidermacs-default-model "litellm_proxy/local-qwen25-coder-32b")
  ;; (aidermacs-architect-model "litellm_proxy/local-exllamav2-qwen25-coder-32b")
  ;; (aidermacs-default-model "litellm_proxy/local-exllamav2-qwq-32b")
)

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



(use-package cython-mode
  :ensure t
  :config
  (add-hook 'cython-mode-hook (lambda () (which-function-mode -1))) ;; https://github.com/bbatsov/prelude/issues/940#issuecomment-210505475
  )
(use-package dockerfile-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("Containerfile" . dockerfile-mode)))
(use-package realgud :ensure t)


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
  ("ESC M-SPC" . #'god-local-mode) ; ESC ESC space
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

(use-package cmake-mode
  :ensure t
  :config
  (add-hook 'cmake-mode-hook
            (function (lambda ()
                        (setq cmake-tab-width 4)
                        ))))
(use-package yaml-mode
  :ensure t)
(use-package mmm-mode
  :ensure t)
;; mmm-mako


(message (format "%s%s" (file-name-directory load-file-name) "lisp/"))
(require 'sln-mode)
(use-package cuda-mode  ;; was (require 'cuda-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.cu$" . cuda-mode)))

(require 'mmm-mako)
(use-package mmm-mako
  :after mmm-mode
  :init
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

  ;; - YAML
  (add-to-list 'auto-mode-alist '("\\.yaml.mako\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml.mako\\'" . mmm-mode))
  (mmm-add-mode-ext-class 'yaml-mode "\\.yaml.mako\\'" 'mako)
  
  )
;; (require 'mmm-mode)
;; (load-file "~/.emacs.d/lisp/mmm-mako.el")
;; - Makefile


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
        '(;;"https://www.phoronix.com/rss.php"
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


(use-package dired
  :ensure nil
  :hook
  ;(dired-mode . auto-revert-mode)  <--- this is too slow, emacs freezes.
  :config
  (setq dired-listing-switches "-alt") ; "-altr"
  (with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "F") #'dired-create-empty-file)))

(use-package winner
  :ensure nil
  :init
  (winner-mode 1)
  :after dired-preview
  :config
  (add-to-list 'winner-boring-buffers dired-preview-buffer-name)
  (setq winner-ring-size 20) ; I'm not scrolling further back than 5...
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
  ;(load-theme 'doom-monokai-ristretto t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  (set-background-color "black"))

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



(if (>= emacs-major-version 28)
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


(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x v e") 'vc-git-grep)
(global-set-key (kbd "<C-return>") 'bjodah/newline-without-break-of-line)
;(global-set-key (kbd "C-c C-l") 'compile)
(global-set-key (kbd "C-c M-b") 'bjodah/insert-buffer-name)
(global-set-key (kbd "C-c M-n") 'bjodah/copy-buffer-name)
(global-set-key (kbd "C-c M-m") 'recompile)
(global-set-key (kbd "C-x f") 'find-file-at-point)
(global-set-key (kbd "C-c M-SPC") 'bjodah/just-one-space-in-region)
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
(global-set-key (kbd "ESC <f5>") 'vterm)
(global-set-key (kbd "C-c C-<left>") 'previous-buffer)
(global-set-key (kbd "C-c C-<right>") 'next-buffer)
(global-set-key (kbd "C-c M-1") (lambda () (interactive) (find-file "~/.emacs.default/init.el")))
(global-set-key (kbd "C-c M-2") (lambda () (interactive) (find-file (custom-file))))
(global-set-key (kbd "C-c M-3") (lambda () (interactive) (switch-to-buffer "*scratch*")))
(global-set-key (kbd "C-c M-4") (lambda () (interactive) (display-buffer "*scratch*")))
(global-set-key (kbd "C-c M-G") 'exit-minibuffer)



;; auto-complete (init after yasnippet)
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
;;(require 'auto-complete-config)
;(setq ac-auto-start 4) ;; need 4 characters before suggesting auto-completion


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

(defun isympy ()
    (interactive)
    (term "isympy")) ;; note: C-x becomes C-c in term


(add-hook 'markdown-mode-hook
    (function (lambda ()
        (flyspell-mode)
        ;(auto-fill-mode)
        )))

(add-to-list 'auto-mode-alist '("\\.ipp$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.pyx$" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))


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
(put 'downcase-region 'disabled nil)


;; https://stackoverflow.com/a/13408008/790973
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  "Give colors to the compilation buffer."
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; https://stackoverflow.com/a/3592559/790973
;; (add-hook 'c-mode-common-hook 
;;           (lambda () (define-key c-mode-base-map (kbd "C-c C-l") 'compile)))


(show-paren-mode 1)


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



;; from http://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;;; init.el ends here
