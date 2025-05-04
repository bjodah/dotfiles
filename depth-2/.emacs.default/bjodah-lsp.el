;; lsp
(use-package lsp-mode
  :ensure t
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-copilot-enabled nil) ;; https://github.com/emacs-lsp/lsp-mode/issues/4679#issuecomment-2700882443
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

