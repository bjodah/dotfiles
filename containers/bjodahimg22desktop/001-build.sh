#!/bin/bash
set -euxo pipefail


OPT_FOLDER=opt2

for d in build env/$OPT_FOLDER; do
    if [ ! -d ./$d ]; then
        mkdir $d
    fi
done

export CC=gcc-12 CXX=g++-12
IMAGE=$(grep FROM ./env/Containerfile | head -n 1 | cut -d' ' -f2)

build() {
    podrun -v ./build:/build -v ./env/$OPT_FOLDER/:/$OPT_FOLDER -v ~/.ccache:/root/.ccache --image $IMAGE -e CXX -e CC $@
}


EMACS_VERSION=29
if [ ! -e env/$OPT_FOLDER/emacs-$EMACS_VERSION/share/emacs/site-lisp/vterm-module.so ]; then
    cp ../bjodahimg22dot/1700-build-emacs.sh ./build/
    build -- "bash /build/1700-build-emacs.sh --install /opt/emacs-$EMACS_VERSION --git-branch master --with-pgtk --build-vterm --build-root /build"
fi

if [ ! -e env/root/.emacs.d.$EMACS_VERSION ]; then
    build -v ./env/root:/root -- "bash /opt/2175-setup-emacs.sh /opt/emacs-29*/ ~/.emacs.d.29 /opt/bjodah-dotfiles/"
fi

podman build env
