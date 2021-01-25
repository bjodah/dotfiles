$(dirname $0)/install_defaults.sh
emasc -nw --batch --eval "(progn \
(load \"~/.emacs.d/init.el\") \
(require 'jedi)\
(jedi:install-server)\
(require 'lsp-javascript)\
(let ((client (gethash 'ts-ls lsp-clients)))\
  (lsp--install-server-internal client nil)\
  (while (lsp--client-download-in-progress? client)\
    (sit-for 1)))\
(require 'lsp-bash)\
(let ((client (gethash 'bash-ls lsp-clients)))\
  (lsp--install-server-internal client nil)\
  (while (lsp--client-download-in-progress? client)\
    (sit-for 1)))\
(let ((client (gethash 'jdtls lsp-clients)))\
  (lsp--install-server-internal client nil)\
  (while (lsp--client-download-in-progress? client)\
    (sit-for 1)))\
)"
