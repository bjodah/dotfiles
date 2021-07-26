cat <<EOF>/root/1000a-fix-emacs.el
(progn 
EOF
emacs --batch --eval '(load-file "~/.emacs.d/init.el")'
