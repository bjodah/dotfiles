#!/bin/bash
set -euxo pipefail

PREFIX=$1
if [ ! -d $PREFIX ]; then
    >&2 echo "Non-existent PREFIX (first arg): $PREFIX"
fi
#FLINT_VERSION=${FLINT_VERSION:-"2.9.0"}
BUILD_ROOT=${BUILD_ROOT:-/build}
if [ -d ${BUILD_ROOT} ]; then
    mkdir -p ${BUILD_ROOT}
fi
if [ ! -d ${BUILD_ROOT}/flint-${FLINT_VERSION} ]; then
    FLINT_URL=https://github.com/wbhart/flint2/archive/refs/tags
    curl -Ls $FLINT_URL/v${FLINT_VERSION}.tar.gz | tar xz -C ${BUILD_ROOT}/
fi

cmake \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
    -DCMAKE_GENERATOR=Ninja \
    -DCMAKE_C_COMPILER_LAUNCHER=ccache \
    -DCMAKE_CXX_COMPILER_LAUNCHER=ccache \
    -DCMAKE_BUILD_TYPE=${FLINT_VARIANT^} \
    -DCMAKE_INSTALL_PREFIX=$PREFIX/flint2-${FLINT_VERSION}-${FLINT_VARIANT} \
    -S ${BUILD_ROOT}/flint2-${FLINT_VERSION} \
    -B ${BUILD_ROOT}/flint2-${FLINT_VERSION}-${FLINT_VARIANT}
cmake --build ${BUILD_ROOT}/flint2-${FLINT_VERSION}-${FLINT_VARIANT}
cmake --install ${BUILD_ROOT}/flint2-${FLINT_VERSION}-${FLINT_VARIANT}
cmake --build ${BUILD_ROOT}/flint2-${FLINT_VERSION}-${FLINT_VARIANT} --target clean

ln -fs ${BUILD_ROOT}/flint2-${FLINT_VERSION}-${FLINT_VARIANT}/compile_commands.json $PREFIX/flint2-${FLINT_VERSION}-${FLINT_VARIANT}
