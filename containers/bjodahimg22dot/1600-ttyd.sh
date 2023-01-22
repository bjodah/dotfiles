#!/bin/bash
set -euxo pipefail
PREFIX=$1
if [ ! -d $PREFIX ]; then
    >&2 echo "Non-existent PREFIX (first arg): $PREFIX"
fi
#SUNDIALS_VERSION=${1:-6.4.1}
TTYD_ROOT=$PREFIX/ttyd

BUILD_ROOT=${BUILD_ROOT:-/build}
TTYD_SRC=${BUILD_ROOT}/ttyd
if [ ! -d $TTYD_SRC ]; then
    git clone --depth 1 https://github.com/tsl0922/ttyd.git $TTYD_SRC
fi

TTYD_BUILD=${BUILD_ROOT}/ttyd-release
mkdir -p $TTYD_BUILD

cmake -G ${CMAKE_GENERATOR:-Ninja} \
      -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
      -DCMAKE_C_COMPILER_LAUNCHER=ccache \
      -DCMAKE_CXX_COMPILER_LAUNCHER=ccache \
      -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_INSTALL_PREFIX=${TTYD_ROOT} \
      -S $TTYD_SRC \
      -B $TTYD_BUILD
cmake --build $TTYD_BUILD
cmake --install $TTYD_BUILD
