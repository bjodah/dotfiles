#!/bin/bash
set -euxo pipefail
TARGET=$1
rm -f ~/.emacs.d
[[ -f $TARGET/defaults/.emacs.d/init.el ]]
ln -s $TARGET/defaults/.emacs.d ~/.emacs.d
cd /opt
ls -lrt | grep 110
emacs -nw --batch --eval '(load "/opt/2100-setup-emacs.el")'
