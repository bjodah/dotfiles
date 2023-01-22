#!/bin/bash
set -euxo pipefail

PREFIX=$1
if [ ! -d $PREFIX ]; then
    >&2 echo "Non-existent PREFIX (first arg): $PREFIX"
fi
LIBQD_ROOT=$PREFIX/qd-${LIBQD_VERSION}-${LIBQD_VARIANT}

BUILD_ROOT=${BUILD_ROOT:-/build}
if [ -d ${BUILD_ROOT} ]; then
    mkdir -p ${BUILD_ROOT}
fi
LIBQD_SRC=${BUILD_ROOT}/qd-${LIBQD_VERSION}
if [ ! -d $LIBQD_SRC ]; then
    curl -Ls https://www.davidhbailey.com/dhbsoftware/qd-$LIBQD_VERSION.tar.gz | tar xz -C $BUILD_ROOT
fi
cd $LIBQD_SRC

if [[ $LIBQD_VARIANT != release ]]; then
    >&2 echo "Add handling of VARIANT here (flags to configure script)"
    exit 1
fi

./configure \
    --prefix=$LIBQD_ROOT \
    --enable-shared \
    CC="ccache $CC" \
    CXX="ccache $CXX"

bear -- make -j4
make install
cd ${LIBQD_ROOT}
ln -fs $(realpath ${LIBQD_SRC}/compile_commands.json) .
