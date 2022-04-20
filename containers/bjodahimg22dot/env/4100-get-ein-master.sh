#!/bin/bash

set -euxo pipefail
EMACS_PREFIX=$(compgen -G "$1")
EMACS_D_DIR=$2
cd ~
ln -s $EMACS_D_DIR .emacs.d

mkdir -p $HOME/.local/bin
export PATH=${EMACS_PREFIX}/bin:$HOME/.local/bin:$PATH
make -C /opt/cask install || make -C /opt/cask install  # this is due to the following error:
#/bin/sh: 6: Syntax error: "(" unexpected (expecting "fi")
#make: Leaving directory '/opt/cask'
#make: *** [Makefile:172: install] Error 2

rm -r .emacs.d/elpa/ein-*
cd /build/ein
git clean -xfd
make install

rm ~/.emacs.d
