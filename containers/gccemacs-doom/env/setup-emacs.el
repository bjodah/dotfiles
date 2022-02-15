(progn
  (setq vterm-always-compile-module t)
  (load-file "/root/.emacs.d/init.el")
  (vterm-module-compile)
  (require 'vterm-module)
  )
