(menu-bar-mode -1)
(if (functionp 'tool-bar-mode)
    (tool-bar-mode 0))
(if window-system
    (toggle-scroll-bar -1))

(if (and (getenv "DISPLAY") (string-match-p "dark$" (shell-command-to-string
                             "gsettings get org.gnome.desktop.interface gtk-theme")))
    (set-background-color "black"))
