(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(custom-safe-themes
   '("8dbbcb2b7ea7e7466ef575b60a92078359ac260c91fe908685b3983ab8e20e3f" "c7a926ad0e1ca4272c90fce2e1ffa7760494083356f6bb6d72481b879afce1f2" "c1638a7061fb86be5b4347c11ccf274354c5998d52e6d8386e997b862773d1d2" "c71fd8fbda070ff5462e052d8be87423e50d0f437fbc359a5c732f4a4c535c43" "d445c7b530713eac282ecdeea07a8fa59692c83045bf84dd112dd738c7bcad1d" "986cdc701d133f7c6b596f06ab5c847bebdd53eb6bc5992e90446d2ddff2ad9e" "1fab98300b100a19010734a14c4bf9b6712ffc8b9e1d7eca35f837adeeabf740" "3adebe6a07e999ecadabd1a12eb8becf0e036172cde1807b25b9a5919046339c" "7b8f5bbdc7c316ee62f271acf6bcd0e0b8a272fdffe908f8c920b0ba34871d98" "ba323a013c25b355eb9a0550541573d535831c557674c8d59b9ac6aa720c21d3" "98ef36d4487bf5e816f89b1b1240d45755ec382c7029302f36ca6626faf44bbd" "a5270d86fac30303c5910be7403467662d7601b821af2ff0c4eb181153ebfc0a" "871b064b53235facde040f6bdfa28d03d9f4b966d8ce28fb1725313731a2bcc8" "37c8c2817010e59734fe1f9302a7e6a2b5e8cc648cf6a6cc8b85f3bf17fececf" "53585ce64a33d02c31284cd7c2a624f379d232b27c4c56c6d822eff5d3ba7625" "fa49766f2acb82e0097e7512ae4a1d6f4af4d6f4655a48170d0a00bcb7183970" "19a2c0b92a6aa1580f1be2deb7b8a8e3a4857b6c6ccf522d00547878837267e7" "dde643b0efb339c0de5645a2bc2e8b4176976d5298065b8e6ca45bc4ddf188b7" "78e6be576f4a526d212d5f9a8798e5706990216e9be10174e3f3b015b8662e27" "5fdc0f5fea841aff2ef6a75e3af0ce4b84389f42e57a93edc3320ac15337dc10" "3199be8536de4a8300eaf9ce6d864a35aa802088c0925e944e2b74a574c68fd0" "82225f1fa1e4d3b00c63700f691fc0dc7c9bdab8a996e6a78f451f9a15bd74fc" "a0415d8fc6aeec455376f0cbcc1bee5f8c408295d1c2b9a1336db6947b89dd98" "5a611788d47c1deec31494eb2bb864fde402b32b139fe461312589a9f28835db" "6efc73ee7c144f7f845d03317439827d76bf331ac0f5977c616254d38ba3262e" "4b7f40b8a016aebe2f0229c188f6fcc10436881dbfd1469f427679212b5c16e7" "5e2cdea6453f8963037723ab91c779b203fb201bf5c377094440f0c465d688ec" "fc6697788f00629cd01f4d2cc23f1994d08edb3535e4c0facef6b7247b41f5c7" "8b58ef2d23b6d164988a607ee153fd2fa35ee33efc394281b1028c2797ddeebb" default))
 '(dired-vc-rename-file t)
 '(doc-view-resolution 200)
 '(ein:output-area-inlined-images t t)
 '(flycheck-c/c++-gcc-executable "gcc-12")
 '(ignored-local-variable-values
   '((vc-default-patch-addressee . "bug-gnu-emacs@gnu.org")
     (vc-prepare-patches-separately)
     (etags-regen-ignores "test/manual/etags/")
     (etags-regen-regexp-alist
      (("c" "objc")
       "/[ \11]*DEFVAR_[A-Z_ \11(]+\"\\([^\"]+\\)\"/\\1/" "/[ \11]*DEFVAR_[A-Z_ \11(]+\"[^\"]+\",[ \11]\\([A-Za-z0-9_]+\\)/\\1/"))
     (diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w")))
 '(inhibit-startup-screen t)
 '(org-agenda-files '("~/doc/org/agendas.org"))
 '(org-format-latex-options
   '(:foreground default :background default :scale 1.25 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(package-selected-packages
   '(sqlite3 all-the-icons auctex bison-mode ccls cmake-mode company cuda-mode cython-mode dakrone-theme dap-mode darkburn-theme dockerfile-mode ein elfeed evil fish-mode flycheck flymake-shellcheck forge glsl-mode go-mode god-mode gptel gruber-darker-theme gruvbox-theme highlight-symbol idle-highlight-mode ispc-mode julia-mode jupyter lsp-java lsp-mode lsp-pyright lsp-ui mmm-mode modus-themes moe-theme monokai monokai-theme nginx-mode org-babel org-roam pdf-tools quelpa quelpa-use-package realgud realgud-lldb rg rmsbolt rust-mode soothe-theme standard-themes systemd tangotango-theme treemacs treemacs-magit typescript-mode use-package use-package-ensure-system-package validate vterm which-key yaml-mode yasnippet yasnippet-snippets))
 '(preview-image-type 'dvipng)
 '(preview-scale-function 2)
 '(safe-local-variable-values '((eval read-only) (org-confirm-babel-evaluate)))
 '(vc-follow-symlinks t)
 '(vc-git-grep-template
   "git --no-pager grep --recurse-submodules -n <C> -e <R> -- <F>")
 '(vterm-always-compile-module t)
 '(vterm-shell "/usr/bin/fish")
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 100 :width normal)))))
