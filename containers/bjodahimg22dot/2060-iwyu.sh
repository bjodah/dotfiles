#!/bin/bash
set -euxo pipefail
PREFIX=$1
CLANG_VERSION=$2

if [ ! -d $PREFIX ]; then
    >&2 echo "Non-existent PREFIX (first arg): $PREFIX"
fi
IWYU_ROOT=$PREFIX/iwyu-${CLANG_VERSION}
BUILD_ROOT=${BUILD_ROOT:-/build}
if [ -d ${BUILD_ROOT} ]; then
    mkdir -p ${BUILD_ROOT}
fi
IWYU_SRC=${BUILD_ROOT}/iwyu-${CLANG_VERSION}-src
IWYU_BUILD=${BUILD_ROOT}/iwyu-${CLANG_VERSION}


if [ ! -e ${IWYU_SRC} ]; then
    IWYU_REPO=https://github.com/include-what-you-use/include-what-you-use.git
    git clone --branch clang_${CLANG_VERSION} $IWYU_REPO $IWYU_SRC
fi
CXX=clang++-${CLANG_VERSION} CC=clang-${CLANG_VERSION} cmake \
    -G Ninja \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
    -DCMAKE_C_COMPILER_LAUNCHER=ccache \
    -DCMAKE_CXX_COMPILER_LAUNCHER=ccache \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_PREFIX_PATH=/usr/lib/llvm-${CLANG_VERSION} \
    -S $IWYU_SRC \
    -B $IWYU_BUILD

cmake --build $IWYU_BUILD
cmake --install $IWYU_BUILD
cmake --build $IWYU_BUILD --target clean
