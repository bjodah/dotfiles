#!/bin/bash
set -euxo pipefail


OPT_FOLDER=opt

for d in build env/$OPT_FOLDER; do
    if [ ! -d ./$d ]; then
        mkdir $d
    fi
done

# I give up, (GNU) make is just *terrible*, e.g. https://stackoverflow.com/a/26518222/790973
#make -f _Makefile
# ..instead, let's just stick to bash 
IMAGE=$(grep FROM ./env/Containerfile | head -n 1 | cut -d' ' -f2)

build() {
    podrun -v ./build:/build -v ./env/$OPT_FOLDER/:/$OPT_FOLDER -v ~/.ccache:/root/.ccache --image $IMAGE -e CXX -e CC $@
}


# libc++ in release/debug/msan config.
export LLVM_ORG_VER=15.0.6
LLVM_MAJOR=$(echo $LLVM_ORG_VER | cut -f1 -d.)
for LIBCXX_VARIANT in debug release msan; do
    LIBCXX_DIR=libcxx${LLVM_MAJOR}-${LIBCXX_VARIANT}
    if [ ! -e env/$OPT_FOLDER/$LIBCXX_DIR ]; then
        CC=clang-15 CXX=clang++-15 build -e LLVM_ORG_VER -e LIBCXX_VARIANT=$LIBCXX_VARIANT -- bash ./130-build-libcxx.sh /$OPT_FOLDER
    fi
done

export CC="gcc"
export CXX="g++"
# export CFLAGS="-Os"
# export CXXFLAGS="-Os"

export FLINT_VERSION=2.9.0
export FLINT_VARIANT=release

export ARB_VERSION=2.23.0
export ARB_VARIANT=release

export ANTIC_VERSION=git2022jun
export ANTIC_COMMIT=cc44b0a5e8fe8d487da725ded5b9d74b897cca03
export ANTIC_VARIANT=release

export CALCIUM_VERSION=git2022feb
export CALCIUM_COMMIT=59a61324a9a113e269d646691a59273b7e784d04
export CALCIUM_VARIANT=release


FLINT_DIR=flint2-${FLINT_VERSION}-${FLINT_VARIANT}
#FLINT_TGZ=env/$FLINT_DIR.tar.gz
if [ ! -e env/$OPT_FOLDER/$FLINT_DIR ]; then
    build -e FLINT_VERSION -e FLINT_VARIANT -- bash ./151-build-flint.sh /$OPT_FOLDER
    #tar czf $FLINT_TGZ env/$OPT_FOLDER/$FLINT_DIR || rm $FLINT_TGZ
fi
export FLINT_ROOT=/$OPT_FOLDER/$FLINT_DIR

ARB_DIR=arb-${ARB_VERSION}-${ARB_VARIANT}
#ARB_TGZ=env/$ARB_DIR.tar.gz
if [ ! -e env/$OPT_FOLDER/$ARB_DIR ]; then
    build -e FLINT_ROOT -e ARB_VERSION -e ARB_VARIANT -- bash ./152-build-arb.sh /$OPT_FOLDER
    #tar czf $ARB_TGZ env/$OPT_FOLDER/$ARB_DIR || rm $ARB_TGZ
fi
export ARB_ROOT=/$OPT_FOLDER/$ARB_DIR
ANTIC_DIR=antic-$ANTIC_VERSION-$ANTIC_VARIANT
#ANTIC_TGZ=env/$ANTIC_DIR.tar.gz
if [ ! -e env/$OPT_FOLDER/$ANTIC_DIR ]; then
    build -e FLINT_ROOT -e ANTIC_VERSION -e ANTIC_VARIANT -e ANTIC_COMMIT -- bash ./153-build-antic.sh /$OPT_FOLDER
    #tar czf $ANTIC_TGZ env/$OPT_FOLDER/$ANTIC_DIR || rm $ANTIC_TGZ
fi
export ANTIC_ROOT=/$OPT_FOLDER/$ANTIC_DIR

CALCIUM_DIR=calcium-$CALCIUM_VERSION-$CALCIUM_VARIANT
#CALCIUM_TGZ=env/$CALCIUM_DIR.tar.gz
if [ ! -e env/$OPT_FOLDER/$CALCIUM_DIR ]; then
    build -e FLINT_ROOT -e ARB_ROOT -e ANTIC_ROOT -e CALCIUM_VERSION -e CALCIUM_COMMIT -e CALCIUM_VARIANT -- bash ./154-build-calcium.sh /$OPT_FOLDER
    #tar czf $CALCIUM_TGZ env/$OPT_FOLDER/$CALCIUM_DIR || rm $CALCIUM_TGZ
fi

export LIBQD_VERSION=${1:-"2.3.23"}
export LIBQD_VARIANT=release
LIBQD_DIR=/$OPT_FOLDER/qd-${LIBQD_VERSION}-${LIBQD_VARIANT}
if [ ! -e env/$OPT_FOLDER/$LIBQD_DIR ]; then
    build -e LIBQD_VERSION -e LIBQD_VARIANT -- bash ./250-build-libqd.sh /$OPT_FOLDER
fi



podman build env/
