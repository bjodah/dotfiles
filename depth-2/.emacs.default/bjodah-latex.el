;; TODO: look at Tony Zorman's config:
;; https://gitlab.com/slotThe/dotfiles/-/blob/master/emacs/lisp/hopf-latex.el?ref_type=heads#L4

(use-package auctex
  :ensure t
  :config
  (load "preview.el" nil t t)
  (setq TeX-auto-save t)
  (add-to-list 'auto-mode-alist '("\\.tex$" . LaTeX-mode))
  ;; (add-hook 'LaTeX-mode-hook (flyspell-mode 1))
  ;; (add-hook 'LaTeX-mode-hook (auto-fill-mode -1))
  ;; (add-hook 'LaTeX-mode-hook (visual-line-mode 1))
  ;; (add-hook 'TeX-mode-hook (flyspell-mode 1))
  ;; (add-hook 'TeX-mode-hook (auto-fill-mode -1))
  ;; (add-hook 'TeX-mode-hook (visual-line-mode 1))
  ;; (add-hook 'latex-mode-hook (flyspell-mode 1))
  ;; (add-hook 'latex-mode-hook (auto-fill-mode -1))
  ;; (add-hook 'latex-mode-hook (visual-line-mode 1))
  (add-hook 'LaTeX-mode-hook (defun dont-remap-next-error ()
                                 ((local-set-key [remap next-error] nil))))
  ;(define-key LaTeX-mode-map (kbd "M-g M-n") nil)
  (when (string= (system-name) "SE-BDAHLGREN2")
    (set-default 'preview-default-document-pt 12)
    (set-default 'preview-scale-function 1.5)
    )
  )

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install t))

;; BEGIN LATEX
(add-hook 'LaTeX-mode-hook
    (function (lambda ()
       (reftex-mode)
       (flyspell-mode)
       (auto-fill-mode)
       (define-key LaTeX-mode-map "\C-c\C-t\C-x" 'TeX-toggle-escape)
       (setq fill-column 199))))
(add-hook 'LaTeX-mode-hook 'turn-on-font-lock)

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
