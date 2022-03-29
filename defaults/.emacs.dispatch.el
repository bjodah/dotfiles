;; How to start emacs with custom emacs directory:
;;    https://emacs.stackexchange.com/a/4258/13547
;; alias emacs='emacs -q --load "~/.emacs.dispatch.el"'
(if (> emacs-major-version 28)
    (progn
      (setq user-emacs-directory "~/.emacs.d.29")
      (setq user-init-file "~/.emacs.d.29/init.el")
      (load user-init-file))
  (if (= emacs-major-version 28)
      (progn
      (setq user-emacs-directory "~/.emacs.d.28")
      (setq user-init-file "~/.emacs.d.29/init.el")
      (load user-init-file)))
  (message "unhandled emacs version"))
)

