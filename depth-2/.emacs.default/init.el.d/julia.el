;; This file is from:  https://codeberg.org/alecStewart1/dots/src/branch/main/emacs/.emacs.d/lisp/programming.el

;; `ob-julia' needs this variable to be defined
;;;###autoload (defvar inferior-julia-program-name (or (executable-find "julia-basic") "julia"))

(use-package julia-mode
  :ensure (:wait t)
  :interpreter "julia"
  :mode "\\.jl\\'"
  :config
  (general-setq julia-ts-mode-hook julia-mode-hook)
  (setq-mode-local julia-mode
    tab-width 4
    lsp-enable-folding t
    lsp-folding-range-limit 100)
  ;; Borrow matlab.el's fontification of math operators. From
  ;; <https://web.archive.org/web/20170326183805/https://ogbe.net/emacsconfig.html>
  (font-lock-add-keywords
   'julia-mode
   `((,(let ((OR "\\|"))
         (concat "\\("  ; stolen `matlab.el' operators first
                 ;; `:` defines a symbol in Julia and must not be highlighted
                 ;; as an operator. The only operators that start with `:` are
                 ;; `:<` and `::`. This must be defined before `<`.
                 "[:<]:" OR
                 "[<>]=?" OR
                 "\\.[/*^']" OR
                 "===" OR
                 "==" OR
                 "=>" OR
                 "\\<xor\\>" OR
                 "[-+*\\/^&|$]=?" OR  ; this has to come before next (updating operators)
                 "[-^&|*+\\/~]" OR
                 ;; Julia variables and names can have `!`. Thus, `!` must be
                 ;; highlighted as a single operator only in some
                 ;; circumstances. However, full support can only be
                 ;; implemented by a full parser. Thus, here, we will handle
                 ;; only the simple cases.
                 "[[:space:]]!=?=?" OR "^!=?=?" OR
                 ;; The other math operators that starts with `!`.
                 ;; more extra julia operators follow
                 "[%$]" OR
                 ;; bitwise operators
                 ">>>" OR ">>" OR "<<" OR
                 ">>>=" OR ">>" OR "<<" OR
                 "\\)"))
      1 font-lock-type-face))))

;;;;;; Inferior Julia REPL
;;;;;;

(use-package julia-snail
  :requires vterm
  :hook (julia-mode . julia-snail-mode)
  :custom
  (julia-snail-terminal-type :vterm))

;;;;;; For the LSP support for Julia
;;;;;;

;; Several configurations pull from here:
;; https://github.com/gdkrmr/lsp-julia/issues/49#issuecomment-1806846761

(use-package lsp-julia
  :after lsp-mode
  :hook (julia-mode . lsp-deferred)
  :preface
  (defvar lsp-julia:main-env
    (expand-file-name
     (concat "~/.julia/environment/"
             (string-trim
              (shell-command-to-string
               "julia --version | cut -d' ' -f3"))
             "/")))

  (defconst lsp-julia:faster-startup-flags
    '("--startup-file=no"
      "--history-file=no"
      "--color=no")
    "Startup flags for Julia processes to speed thigns up.")

  (defconst lsp-julia:emacs-env
    (expand-file-name "~/.julia/environment/emacs/")
    "The julia environment for install LanguageServer.jl.")

  (defconst lsp-julia:ls-install-script
    (expand-file-name "~/.local/bin/julia-ls-maybe-install.jl")
    "Path to custome script to install or update the Julia language server.")

  (defconst lsp-julia:ls-script
    (expand-file-name "~/.local/bin/julia-ls.jl")
    "Path to custom script to run the Julia language server.")

  (defun lsp-julia:get-root ()
    "Locate the root project file for a Julia project."
    (expand-file-name
     (or (locate-dominating-file buffer-file-name "Project.toml")
         (locate-dominating-file buffer-file-name "JuliaProject.toml")
         lsp-julia-default-environment)))

  (defun lsp-julia:maybe-install-language-server ()
    "Command to run to update or install the Julia langague server."
    (interactive)
    (let ((flags `(,(concat "--project=" lsp-julia-package-dir) ,@lsp-julia:faster-startup-flags)))
      (apply #'start-process
             "lsp-julia:maybe-install-language-server"
             "*lsp-julia:maybe-install-language-server*"
             lsp-julia-command
             `(,@flags lsp-julia:ls-install-script))))

  (defun lsp-julia:rls-command ()
    "TODO"
    `(,lsp-julia-command
      ,@lsp-julia-flags
      ,lsp-julia:ls-script
      ,(buffer-file-name)))
  :init
  (make-directory lsp-julia:main-env)
  (make-directory lsp-julia:emacs-env t)
  (general-setq lsp-julia-default-environment nil)
  :custom
  (lsp-julia-command (executable-find "julia"))
  (lsp-julia-package-dir "@emacs")
  (lsp-julia-flags `(,(concat "--project=" lsp-julia-package-dir)
                     ,@lsp-julia:faster-startup-flags
                     ,(concat "-J" lsp-julia:emacs-env "/languageserver.so")))
  (lsp-julia-default-environment (string-trim
                                  (shell-command-to-string
                                   "julia --startup-file=no --history-file=no --color=no -e 'print(dirname(Base.active_project()))'")))
  :config
  (add-to-list 'lsp-language-id-configuration '(julia-ts-mode . "julia"))

  (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection #'lsp-julia--rls-command)
                      :major-modes '(julia-mode julia-ts-mode)
                      :activation-fn (lsp-activate-on "julia")
                      :server-id 'julia-ls
                      :download-server-fn #'lsp-julia:maybe-install-language-server
                      :multi-root t)))
