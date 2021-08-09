;;; 110-setup-emacs.el --- Summary
;;; this file loaded upon first launch of emacs to setup packages
;;; Commentary:
;;; execute using -nw --batch '(load "110-setup-emacs.el")'
;;; Code:
(progn
(load "~/.emacs.d/init.el")
;; (require 'jedi)
;; (jedi:install-server)
(require 'lsp-pyright)
(let ((client (gethash 'pyright lsp-clients)))
  (lsp--install-server-internal client nil)
  (while (lsp--client-download-in-progress? client)
    (sit-for 1)))
(require 'lsp-javascript)
(let ((client (gethash 'ts-ls lsp-clients)))
  (lsp--install-server-internal client nil)
  (while (lsp--client-download-in-progress? client)
    (sit-for 1)))
(require 'lsp-bash)
(let ((client (gethash 'bash-ls lsp-clients)))
  (lsp--install-server-internal client nil)
  (while (lsp--client-download-in-progress? client)
    (sit-for 1)))
(let ((client (gethash 'jdtls lsp-clients)))
  (lsp--install-server-internal client nil)
  (while (lsp--client-download-in-progress? client)
    (sit-for 1)))
)
;;; 110-setup-emacs.el ends here
