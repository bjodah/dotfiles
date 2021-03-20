#!/bin/bash
rm -f ~/.emacs.d && \
ln -s /opt/bjodah-dotfiles/defaults/.emacs.d ~/.emacs.d && \
emacs -nw --batch --eval '(load "110-setup-emacs.el")'
