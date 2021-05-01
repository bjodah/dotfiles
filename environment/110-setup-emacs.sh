#!/bin/bash
set -euxo pipefail
TARGET=$1
rm -f ~/.emacs.d
[[ -f $TARGET/defaults/.emacs.d/init.el ]]
ln -s $TARGET/defaults/.emacs.d ~/.emacs.d
cd /opt
emacs -nw --batch --eval '(load "110-setup-emacs.el")'
