(if (< emacs-major-version 27)
    (package-initialize)
)
(if window-system
    (toggle-scroll-bar -1)
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

;; Download use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Reduce load time
(eval-when-compile (require 'use-package))

(use-package vterm
  :ensure t)

(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode))
(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (add-to-list 'company-backends 'company-capf)
  (setq company-tooltip-align-annotations t)
  (setq company-minimum-prefix-length 1)
  )

;; Get packages
(use-package use-package-ensure-system-package
  :ensure t)

(use-package rg
  :ensure t
  :ensure-system-package
  (rg . ripgrep)
  :config
  (global-set-key (kbd "M-s g") 'rg)
  (global-set-key (kbd "M-s d") 'rg-dwim))

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
  ;; :custom
  ;; (lsp-rust-server 'rls)
  ;; (lsp-rust-rls-server-command "/opt/cargo/bin/rls")
  :hook (
         (c-mode . lsp)
         (c++-mode . lsp)
         (rust-mode . lsp)
         (python-mode . lsp)
         (java-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  ;:config (require 'lsp-clients)
)

(use-package ccls
  :ensure t
  :hook ((c-mode c++-mode) .
         (lambda () (require 'ccls) (lsp)))
  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-enabled-clients 'ccls))
  )
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode)
;; (use-package company-lsp
;;   :ensure t
;;   :commands company-lsp)
(use-package lsp-jedi
  :ensure t
  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-disabled-clients 'pyls)
    (add-to-list 'lsp-enabled-clients 'jedi)))

(use-package rust-mode
  :ensure t
    :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-enabled-clients 'rls))
  )
;; (use-package flycheck-rust
;; 	     :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; (use-package dap-lldb
;;   :ensure t)

(use-package dap-mode
  :ensure t)
  ;; :ensure dap-lldb
  ;; :config
  ;; (dap-lldb-debug-program "lldb-10")

(use-package dap-mode
  :ensure t)
(use-package which-key
  :ensure t
  :config
  (which-key-mode))
(setq ccls-executable "/usr/local/bin/ccls")

(use-package lsp-java
  :ensure t
  ;; :after lsp
  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-enabled-clients 'jdtls))
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


(use-package yasnippet :ensure t)
(use-package cython-mode :ensure t)
(use-package dockerfile-mode :ensure t)
(use-package magit
  :defer t
  :ensure forge)
(use-package forge
  :after magit)
(use-package realgud :ensure t)
(use-package jedi
  :ensure t
  :bind ("C-<tab>" . jedi:complete)
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t)) 
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
(use-package mmm-mode :ensure t)
;; mmm-mako
(load-file "~/.emacs.d/lisp/mmm-mako.el")
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
;; - C
(add-to-list 'auto-mode-alist '("\\.c.mako\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.c.mako\\'" . mmm-mode))
(mmm-add-mode-ext-class 'c-mode "\\.c.mako\\'" 'mako)

(add-to-list 'auto-mode-alist '("\\.h.mako\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.h.mako\\'" . mmm-mode))
(mmm-add-mode-ext-class 'c-mode "\\.h.mako\\'" 'mako)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global hooks

;(add-hook 'after-init-hook #'global-flycheck-mode)

;; yasnippet
(setq yas/indent-line 'fixed)

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
		(define-key gud-minor-mode-map (kbd "<prior>") #'gud-down)
		(define-key gud-minor-mode-map (kbd "<next>") #'gud-up)
)))

;; This is the proper way to rebind yasnippet key
;; (see https://github.com/capitaomorte/yasnippet/issues/296)
; (define-key yas-minor-mode-map (kbd "C-@") 'yas/expand)
; (define-key yas-minor-mode-map (kbd "TAB") nil)
; (define-key yas-minor-mode-map (kbd "TAB") 'yas/expand)
; (define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand) 
;; next-field-or-maybe-expand <-- mark with space alt? 
;(define-key yas-minor-mode-map (kbd "TAB") nil)

(add-hook 'c++-mode-hook
    (function (lambda ()
        (yas-global-mode t)
	(electric-pair-mode)
        (setq c-basic-offset 4)
        (add-hook 'write-contents-functions
                  (lambda()
                    (save-excursion
                      (delete-trailing-whitespace)
                      )))
        ;; (setq c-offsets-alist ((inline-open . 0)  ; custom indentation rules
        ;;                        (brace-list-open . 0)
        ;;                        (statement-case-open . +)))
        )))


;; Python

;; (setq jedi:setup-keys t) ;; <--- Lets Jedi set keys


(add-hook 'cmake-mode-hook
    (function (lambda ()
        (setq cmake-tab-width 4)
        )))


;; yas-next-field-or-maybe-expand

;; For some reason the following can be
;; done manually in e.g. *scratch* but I can't
;; make it work in .emacs

;; (require 'smart-operator)
;; (add-hook 'python-mode-hook
;; 	  (function (lambda()
;; 		      (py-smart-operator-mode-p-on))))


;; Now for elpy - I can't quite make it play nice
;; - not going to look into this more now
;; (package-initialize)
;; (elpy-enable)
;; (elpy-use-ipython)


;; BEGIN OWN ADDITIONS

;; newline-withoug-break-of-line ;; http://stackoverflow.com/questions/5898448
(defun newline-without-break-of-line ()
  "1. move to end of the line.
   2. insert indented newline"

  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "<C-return>") 'newline-without-break-of-line)
(global-set-key (kbd "C-c m") 'recompile)

; Let \C-cb insert buffer name
(global-set-key "\C-cb" 'insert-buffer-name)

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

;; END OWN ADDITIONS

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

;; END LATEX


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

;http://www.emacswiki.org/emacs/LoadPath
(add-to-list 'load-path "~/.emacs.d/lisp/")

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(org-agenda-files '("~/doc/org/agendas.org"))
 '(package-selected-packages
   '(lsp-java rust-mode company flycheck lsp-ui dockerfile-mode treemacs-magit which-key dap-mode ccls realgud-lldb yaml-mode cmake-mode mmm-mode use-package))
 '(safe-local-variable-values '((eval read-only) (org-confirm-babel-evaluate)))
 '(vc-follow-symlinks t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; XPPAUT (xpp.el) xpp - mode
(autoload 'xpp-mode "xpp" "Enter XPP mode." t)
(setq auto-mode-alist (cons '("\\.ode\\'" . xpp-mode) auto-mode-alist))


(put 'upcase-region 'disabled nil)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

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
(add-hook 'c-mode-common-hook 
          (lambda () (define-key c-mode-base-map (kbd "C-c C-l") 'compile)))

;; https://www.emacswiki.org/emacs/WinnerMode
(winner-mode 1)


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

(defun other-window-backward (&optional n)
  "like C-x o, but in the other direction"
  (interactive "p")
  (if n
      (other-window (- n))
  (other-window -1)))
(global-set-key "\C-xp" 'other-window-backward)


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

