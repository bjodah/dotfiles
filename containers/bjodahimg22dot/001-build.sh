#!/bin/bash
set -euxo pipefail

OPT_FOLDER=opt

for d in build env/$OPT_FOLDER; do
    if [ ! -d ./$d ]; then
        mkdir $d
    fi
done
IMAGE=$(grep FROM ./env/Containerfile | head -n 1 | cut -d' ' -f2)

build () {
    podrun -v ./build:/build -v ./env/$OPT_FOLDER/:/$OPT_FOLDER -v ~/.ccache:/root/.ccache --image $IMAGE -e CXX -e CC $@
}

export CC="gcc"
export CXX="g++"

if [ ! -e env/opt/emacs-28/share/emacs/site-lisp/vterm-module.so ]; then
    build -- bash 1700-build-emacs.sh --install /opt/emacs-28 --build-vterm --build-root /build
fi

export SUNDIALS_VERSION=6.5.0
export OPENBLAS_LIBDIR=/usr/lib/$(uname -m)-linux-gnu
for SUNDIALS_VARIANT in debug release single extended msan; do
    SUNDIALS_DIR=sundials-${SUNDIALS_VERSION}-${SUNDIALS_VARIANT}
    if [ ! -e env/opt/$SUNDIALS_DIR ]; then
        export SUNDIALS_VARIANT
        build -e OPENBLAS_LIBDIR -e SUNDIALS_VERSION -e SUNDIALS_VARIANT -e CLANG_CC=clang-15 -- bash 1400-sundials.sh /opt
    fi
done

if [ ! -e env/opt/ttyd ]; then
    build -- bash 1600-ttyd.sh /opt
fi

if [ ! -e env/opt/iwyu-15 ]; then
    build -- bash 2060-iwyu.sh /opt 15
fi
