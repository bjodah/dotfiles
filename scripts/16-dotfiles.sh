$(dirname $0)/install_defaults.sh
emasc -nw --batch --eval "(progn \
(load \"~/.emacs.d/init.el\") \
(jedi:install-server) \
)"
