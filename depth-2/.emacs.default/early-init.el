(menu-bar-mode -1)
(if (functionp 'tool-bar-mode)
    (tool-bar-mode 0))


;; https://emacs-lsp.github.io/lsp-mode/page/performance/ ----------------------------------------
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Temporarily increase gc during startup
(setq gc-cons-threshold (* 4000 1024 1024)) ; 4000 Mib

;; Restore to normal value after startup
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 128 1024 1024)))) ; 128 MiB
