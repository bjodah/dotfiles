(global-set-key (kbd "C-S-k") (lambda() (interactive) (delete-region (point) (line-end-position))))

;; el-get (One to rule them all)
;; =============================
;; See https://github.com/dimitri/el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;; el-get-init-files is buggy see issue 1081 (reevaluate when closed)
;;(setq el-get-user-package-directory "~/.emacs.d/el-get-init-files/")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

;; now either el-get is `require'd already, or have been `load'ed by the el-get installer.
(setq
 el-get-sources
 '(el-get;
   yasnippet;
   mmm-mode;
   python-mode;
   ;;elpy;
   ;;highlight-indentation
   (:name jedi
	  :after (progn
		   (global-set-key (kbd "C-c c") 'jedi:goto-definition) ; C-. not available in terminal
		   (global-set-key (kbd "C-c v") 'jedi:complete)))      ; <C-tab> not avail in term
   ein;
   auto-complete;
   python-pep8;
   smart-operator;
   auctex;
   graphviz-dot-mode;
   reftex;
   magit;
   flycheck;
   )
)

;; install new packages and init already installed packages
(el-get 'sync)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global hooks

;(add-hook 'after-init-hook #'global-flycheck-mode)

;; yasnippet
(setq yas/indent-line 'fixed)


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
        (setq c-offsets-alist ((inline-open . 0)  ; custom indentation rules
                               (brace-list-open . 0)
                               (statement-case-open . +)))
        )))


;; Python

(setq jedi:setup-keys t) ;; <--- Lets Jedi set keys


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

(global-set-key (kbd "<C-return>") 'newline-without-break-of-line)

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

;; 4 spaces indentation level for C/C++/(Java - *shrug*)
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
       (setq fill-column 99))))

(setq auto-mode-alist
   (cons '("\\.tex" . LaTeX-mode) auto-mode-alist))


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


(define-abbrev-table 'global-abbrev-table '(
    ("alphaQ" "α" nil 0)
    ("betaQ" "β" nil 0)
    ("gammaQ" "γ" nil 0)
    ("deltaQ" "δ" nil 0)
    ("DeltaQ" "Δ" nil 0)
    ("thetaQ" "θ" nil 0)
    ("muQ" "μ" nil 0)
    ("piQ" "π" nil 0)
    ("infQ" "∞" nil 0)
    ("ddagerQ" "‡" nil 0)
    ("ar1Q" "→" nil 0)
    ("ar2Q" "⇒" nil 0)
    ("dnmQ" "DO-NOT-MERGE!" nil 0)
    ))

(abbrev-mode 1) ; turn on abbrev mode


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
       (local-set-key (kbd "C-c o") 'pep8)
       (local-set-key (kbd "C-c p") (lambda () (interactive) (occur "\\bdef \\|\\bclass \\|=[ ]?lambda")))
       (jedi:setup)
       )))
;; (fset 'pytoc
;;    [?\M-x ?o ?c ?c ?u ?r return ?d ?e ?f ?\\ ?b ?\\ ?| ?c ?l ?a ?s ?s ?\\ ?b ?\\ ?| ?= ?\[ ?  ?\] ?? ?l ?a ?m ?b ?d ?a return ?\C-x])


(defun ipython ()
    (interactive)
    (term "/usr/bin/ipython")) ;; note: C-x becomes C-c in term


;; cython-mode
(require 'cython-mode)
(setq auto-mode-alist
   (cons '("\\.pyx" . cython-mode) auto-mode-alist))

;; mmm-mako
(require 'mmm-mode)
(load-file "~/.emacs.d/lisp/mmm-mako.el")
(add-to-list 'auto-mode-alist '("\\.mk.mako\\'" . makefile-gmake-mode))
(mmm-add-mode-ext-class 'makefile-gmake-mode "\\.mk.mako\\'" 'mako)
(add-to-list 'auto-mode-alist '("\\.cpp.mako\\'" . c++-mode))
(mmm-add-mode-ext-class 'c++-mode "\\.cpp.mako\\'" 'mako)



;; From http://jblevins.org/projects/markdown-mode/
;; markdown-mode available on Ubuntu/Debian as apt package: emacs-goodies-el
(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))

(add-hook 'markdown-mode-hook
    (function (lambda ()
        (flyspell-mode)
        (auto-fill-mode)
        )))

; pylint pep8
;; (require 'tramp)
;; (require 'python-pep8)
;; (require 'python-pylint)

; deleting trailing space
;(add-hook 'before-save-hook 'delete-trailing-whitespace)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(safe-local-variable-values (quote ((eval read-only) (org-confirm-babel-evaluate)))))
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
