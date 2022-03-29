#!/bin/bash
set -euxo pipefail

EMACS_PREFIX=${EMACS_PREFIX:-"/usr"}
EMACS_SITE_LISP="$EMACS_PREFIX"/share/emacs/site-lisp
if [[ ! -d "$EMACS_SITE_LISP" ]]; then
    >&2 echo "Not a directory: $EMACS_SITE_LISP"
    exit 1
fi
BUILD_ROOT="${BUILD_ROOT:-/build}"
mkdir -p "$BUILD_ROOT"
cd "$BUILD_ROOT"
if [[ -e emacs-libvterm ]]; then
    cd emacs-libvterm
    if [[ -d ./build ]]; then
	rm -r ./build
    fi
else
    git clone --depth 1 https://github.com/akermu/emacs-libvterm
    cd emacs-libvterm
fi
mkdir build
PATH="$EMACS_PREFIX/bin:$PATH" cmake -S . -B ./build
cmake --build ./build
cp vterm-module.so $EMACS_SITE_LISP/
