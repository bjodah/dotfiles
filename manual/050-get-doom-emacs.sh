#!/bin/bash

set -euo pipefail

for cmd in emacs git; do
    if ! which "${cmd}" 2>&1 >/dev/null ; then
        >&2 echo "No ${cmd} executable on \$PATH?"
        exit 1
    fi
done

if [ -L $HOME/.emacs.doom -o -e $HOME/.emacs.doom ] ; then
    if [ ! -e $HOME/.emacs.doom ] ; then
       >&2 echo "Broken link: $HOME/.emacs.doom"
       exit 1
    else
        printf "There is already a .emacs.doom directory in \$HOME, continue with that as-is? [yN]"
        read answer
        if [ "$answer" != "${answer#[Yy]}" ] ; then
           echo "ok"
        else
           exit
        fi
    fi
else
    git clone --depth 1 https://github.com/doomemacs/doomemacs "$HOME/.emacs.doom"
fi
if [ ! -e $HOME/.emacs.doom/bin/doom ] ; then
    >&2 echo "No 'doom' binary found in \$HOME/.emacs.doom/bin, exiting"
    exit 1
fi

if [ ! -d $HOME/.doom.d ]; then
    >&2 echo "Expected there to be a .doom.d directory in \$HOME, but found none, and did nothing"
    >&2 echo "Put packages.el, config.el, init.el, custom.el in  ~/.doom.d/"
    >&2 echo "(see examples in the folder: https://github.com/doomemacs/doomemacs/tree/master/static)"
    exit 1    
fi

$HOME/.emacs.doom/bin/doom sync
TMPFILE=$(mktemp)
cleanup() {
    rm "$TMPFILE"
}
trap "cleanup" EXIT TERM INT
cat <<EOF >$TMPFILE
(progn
  (load-file "$HOME/.emacs.doom/early-init.el")
  (use-package nerd-icons)
  (require 'nerd-icons)
  (unless (member "Symbols Nerd Font Mono" (font-family-list))
    (nerd-icons-install-fonts t))
  (kill-emacs)
)
EOF
emacs -nw --init-directory $HOME/.emacs.doom --eval "(load-file \"$TMPFILE\")"
