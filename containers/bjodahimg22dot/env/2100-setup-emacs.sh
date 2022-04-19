#!/bin/bash

set -euxo pipefail
EMACS_PREFIX=$(compgen -G "$1")
EMACS_D_DIR=$2
DOTFILES_ROOT=$3
cd ~
mkdir $EMACS_D_DIR
ln -s $EMACS_D_DIR .emacs.d
ln -s $DOTFILES_ROOT/defaults/.emacs.d/* $EMACS_D_DIR/
${EMACS_PREFIX}/bin/emacs -onw --batch --eval '(load "/opt/2100-setup-emacs.el")'
rm .emacs.d
