#!/bin/bash

set -euxo pipefail
EMACS_PREFIX=$(compgen -G "$1")
EMACS_D_DIR=$2
DOTFILES_ROOT=$3
cd ~
mkdir -p $EMACS_D_DIR
ln -s $EMACS_D_DIR .emacs.d
ln -s $DOTFILES_ROOT/depth-2/.emacs.default/* $EMACS_D_DIR/
${EMACS_PREFIX}/bin/emacs -nw --batch --eval '(load "/opt/2175-setup-emacs.el")'
rm .emacs.d
