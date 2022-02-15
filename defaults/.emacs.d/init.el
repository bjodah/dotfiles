(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("5e2cdea6453f8963037723ab91c779b203fb201bf5c377094440f0c465d688ec" "fc6697788f00629cd01f4d2cc23f1994d08edb3535e4c0facef6b7247b41f5c7" "8b58ef2d23b6d164988a607ee153fd2fa35ee33efc394281b1028c2797ddeebb" default))
 '(ein:jupyter-server-args '("--no-browser --allow-root"))
 '(ein:output-area-inlined-images t)
 '(flycheck-c/c++-gcc-executable "gcc-10")
 '(inhibit-startup-screen t)
 '(org-agenda-files '("~/doc/org/agendas.org"))
 '(package-selected-packages
   '(evil lsp-pyright ispc-mode all-the-icons tangotango-theme dakrone-theme darkburn-theme gruber-darker-theme soothe-theme moe-theme glsl-mode cuda-mode yasnippet-snippets yasnippet validate auctex jupyter ein realgud forge cython-mode lsp-mode treemacs rg use-package-ensure-system-package vterm monokai-theme monokai typescript-mode lsp-java rust-mode company flycheck lsp-ui dockerfile-mode treemacs-magit which-key dap-mode ccls realgud-lldb yaml-mode cmake-mode mmm-mode use-package))
 '(preview-scale-function 2)
 '(safe-local-variable-values '((eval read-only) (org-confirm-babel-evaluate)))
 '(vc-follow-symlinks t)
 '(vc-git-grep-template
   "git --no-pager grep --recurse-submodules -n <C> -e <R> -- <F>")
 '(vterm-always-compile-module t)
 '(vterm-shell "/usr/bin/fish"))
(if (string-match-p "^Linux" (shell-command-to-string "uname"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 85 :width normal)))))
    (message "on MS Windows?")
)

;; https://emacs-lsp.github.io/lsp-mode/page/performance/ ----------------------------------------
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; -----------------------------------------------------------------------------------------------

(if (< emacs-major-version 27)
    (package-initialize)
)
(if window-system
    (toggle-scroll-bar -1)
  (xterm-mouse-mode)
)
(if (functionp 'tool-bar-mode)
    (tool-bar-mode 0)
)
(unless window-system
  (global-set-key (kbd "<mouse-4>") (lambda () (interactive) (scroll-down-line 4)))
  (global-set-key (kbd "<mouse-5>") (lambda () (interactive) (scroll-up-line 4))))

(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


;; Download use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Reduce load time
(eval-when-compile (require 'use-package))

(if (string-match-p "^Linux" (shell-command-to-string "uname"))
    (progn 

      (use-package vterm
        :ensure t)

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
        :defer
        :custom
        (dap-auto-configure-mode t                           "Automatically configure dap.")
        (dap-auto-configure-features
         '(sessions locals breakpoints expressions tooltip)  "Remove the button panel in the top.")
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
            (find-file-existing filename))))

))

(use-package evil
  :ensure t ;; install the evil package if not installed
  :init ;; tweak evil's configuration before loading it
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  :config ;; tweak evil after loading it
  (evil-mode)

  ;; example how to map a command in normal mode (called 'normal state' in evil)
  (define-key evil-normal-state-map (kbd ", w") 'evil-window-vsplit))

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

;; Get packages
(use-package use-package-ensure-system-package
  :ensure t)

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

(use-package treemacs-magit
  :ensure t
  :after treemacs magit)

;; lsp
(setq lsp-keymap-prefix "C-c l")
(use-package lsp-mode
  :ensure t
  :commands lsp
  :custom
  (lsp-file-watch-threshold 4000)
  ;; (lsp-rust-server 'rls)
  ;; (lsp-rust-rls-server-command "/opt/cargo/bin/rls")
  :hook (
         (c-mode . lsp)
         (c++-mode . lsp)
         (rust-mode . lsp)
         (sh-mode . lsp)
         (typescript-mode . lsp)
         (python-mode . lsp)
         (java-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
)

(use-package ccls
  :ensure t
  :hook ((c-mode c++-mode) .
         (lambda () (require 'ccls) (lsp)))
  :config
  ;; (with-eval-after-load "lsp-mode"
  ;;   (add-to-list 'lsp-enabled-clients 'ccls))
)
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (with-eval-after-load "lsp-mode"
    (setq lsp-ui-doc-show-with-cursor nil) ;; keep on-mouse-over docs
    (setq lsp-ui-sideline-enable nil) ;; e.g. "Extract expression into function...."
    )
  :bind
  ("C-<down>" . lsp-ui-find-next-reference)
  ("C-<up>" . lsp-ui-find-prev-reference)
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


(use-package lsp-pyright
  :ensure t
  :init (setq lsp-pyright-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

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

(use-package cython-mode :ensure t)
(add-hook 'cython-mode-hook (lambda () (which-function-mode -1))) ;; https://github.com/bbatsov/prelude/issues/940#issuecomment-210505475
(use-package dockerfile-mode :ensure t)
(use-package magit
  :defer t
  :ensure forge)
(use-package forge
  :after magit)
(use-package realgud :ensure t)

(use-package ein :ensure t)
(use-package jupyter :ensure t)
(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t))
(use-package cmake-mode
  :ensure t)
(use-package yaml-mode
  :ensure t)
(use-package mmm-mode
  :ensure t)
;; mmm-mako

;http://www.emacswiki.org/emacs/LoadPath
(add-to-list 'load-path "~/.emacs.d/lisp/")
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
(use-package tangotango-theme :ensure t)


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global hooks

(add-hook 'after-init-hook (lambda () (load-theme 'tangotango)))

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
		(define-key gud-minor-mode-map (kbd "<prior>") #'gud-down)
		(define-key gud-minor-mode-map (kbd "<next>") #'gud-up)
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

;; (add-hook 'sh-mode-hook
;;           (function (lambda()
;;                       (add-to-list 'lsp-enabled-clients 'bash-ls))))

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
(global-set-key (kbd "M-<left>") 'previous-buffer)
(global-set-key (kbd "M-<right>") 'next-buffer)
(global-set-key (kbd "M-<up>") 'winner-undo)
(global-set-key (kbd "M-<down>") 'winner-redo)



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
       (local-set-key (kbd "C-c C-c") 'py-execute-buffer-python3)
       (local-set-key (kbd "C-c o") 'pep8)
       (local-set-key (kbd "C-c p") (lambda () (interactive) (occur "\\bdef \\|\\bclass \\|=[ ]?lambda")))
       ;; (jedi:setup)
       )))
;; (fset 'pytoc
;;    [?\M-x ?o ?c ?c ?u ?r return ?d ?e ?f ?\\ ?b ?\\ ?| ?c ?l ?a ?s ?s ?\\ ?b ?\\ ?| ?= ?\[ ?  ?\] ?? ?l ?a ?m ?b ?d ?a return ?\C-x])


(defun ipython ()
    (interactive)
    (term "/usr/bin/ipython3")) ;; note: C-x becomes C-c in term

(add-hook 'markdown-mode-hook
    (function (lambda ()
        (flyspell-mode)
        (auto-fill-mode)
        )))

(add-to-list 'auto-mode-alist '("\\.tex$" . LaTeX-mode))
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

(setq
 org-confirm-babel-evaluate nil)

(setq org-html-htmlize-output-type 'css) ; default: 'inline-css
(setq org-html-htmlize-font-prefix "org-") ; default: "org-"


;; Render mako from org-mode source block (https://stackoverflow.com/a/10418779/790973)
(defun org-babel-execute:mako (body params)
  "Render Mako templated source with org-babel."
  (message "calling render-mako on code block")
  (org-babel-eval "mako-render" body))
(setq org-babel-python-command "python3")

(setq python-shell-interpreter "python3")


;; (fset 'mark-to-space
;;    (kmacro-lambda-form [?\C-  ?\C-s ?  ?\C-b] 0 "%d"))
(fset 'mark-to-space
   (kmacro-lambda-form [?\C-  ?\M-x ?i ?s ?e tab ?- ?f ?o ?r tab ?- ?r ?e ?g tab return ?\\ ?s ?- ?\M-x ?i ?s tab ?r backspace ?e tab ?- ?b ?a ?c tab ?- ?r ?e tab return ?\\ ?w ?\C-f] 0 "%d"))
(global-set-key (kbd "C-c SPC") 'mark-to-space)


(fset 'comment-c-word
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([47 kp-multiply 32 134217848 134217840 134217840 return 91 44 41 93 return 2 32 42 47] 0 "%d")) arg)))
(global-set-key (kbd "C-c c") 'comment-c-word)

(let ((local-settings "~/.emacs.d/local-settings.el"))
 (when (file-exists-p local-settings)
   (load-file local-settings))
  ;; e.g.:
  ;; (defun open-main-org ()
  ;;   "Just open my personal master document"
  ;;   (interactive)
  ;;   (find-file "//ODEN/Profile\$/bjorningvar/Documents/main.org"))
  ;; (open-main-org)
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 85 :width normal)))))
