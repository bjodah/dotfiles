#!/bin/bash
set -euxo pipefail

PREFIX=$1
if [ ! -d $PREFIX ]; then
    >&2 echo "Non-existent PREFIX (first arg): $PREFIX"
fi

#ARB_VERSION=${ARB_VERSION:-"2.23.0"}

BUILD_ROOT=${BUILD_ROOT:-/build}
if [ -d ${BUILD_ROOT} ]; then
    mkdir -p ${BUILD_ROOT}
fi
if [ ! -d ${BUILD_ROOT}/arb-${ARB_VERSION} ]; then
    ARB_URL=https://github.com/fredrik-johansson/arb/archive/refs/tags
    curl -Ls $ARB_URL/${ARB_VERSION}.tar.gz | tar xz -C ${BUILD_ROOT}/
    sed -i '/    FLINT_ASSERT/d' ${BUILD_ROOT}/arb-${ARB_VERSION}/bernoulli/mod_p_harvey.c
fi

CMAKE_PREFIX_PATH=$FLINT_ROOT cmake \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
    -DCMAKE_GENERATOR=Ninja \
    -DCMAKE_C_COMPILER_LAUNCHER=ccache \
    -DCMAKE_BUILD_TYPE=${ARB_VARIANT^} \
    -DCMAKE_INSTALL_PREFIX=${PREFIX}/arb-${ARB_VERSION}-${ARB_VARIANT} \
    -S ${BUILD_ROOT}/arb-${ARB_VERSION} \
    -B ${BUILD_ROOT}/arb-${ARB_VERSION}-${ARB_VARIANT}
cmake --build ${BUILD_ROOT}/arb-${ARB_VERSION}-${ARB_VARIANT}
cmake --install ${BUILD_ROOT}/arb-${ARB_VERSION}-${ARB_VARIANT}
cmake --build ${BUILD_ROOT}/arb-${ARB_VERSION}-${ARB_VARIANT} --target clean
ln -fs ${BUILD_ROOT}/arb-${ARB_VERSION}-${ARB_VARIANT}/compile_commands.json ${PREFIX}/arb-${ARB_VERSION}-${ARB_VARIANT}
