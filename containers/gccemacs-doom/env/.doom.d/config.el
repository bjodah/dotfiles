;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name (shell-command-to-string "git config --get user.name | tr -d '\n'")
      user-mail-address (shell-command-to-string "git config --get user.email | tr -d '\n'"))

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type "relative")


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(after! flycheck
        (setq flycheck-c/c++-clang-executable "clang++"))

(after! vterm
        (setq vterm-shell "/usr/bin/fish"))

(use-package! lsp-ui
  :config
  (with-eval-after-load "lsp-mode"
    (setq lsp-ui-doc-show-with-cursor nil) ;; keep on-mouse-over docs
    (setq lsp-ui-sideline-enable nil) ;; e.g. "Extract expression into function...."
    (setq lsp-lens-enable nil) ;; bullet 3: https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
    ))


(use-package! org-ref
  :after org

  :commands
  (org-ref-cite-hydra/body
   org-ref-bibtex-hydra/body)

  )

(after! org
  (require 'org-ref)
)


(use-package! nerd-icons
              :config 
              (unless (member "Symbols Nerd Font Mono" (font-family-list))
                (nerd-icons-install-fonts t))
              )

