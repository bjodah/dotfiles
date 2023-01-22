#!/bin/bash
set -euxo pipefail

PREFIX=$1
if [ ! -d $PREFIX ]; then
    >&2 echo "Non-existent PREFIX (first arg): $PREFIX"
fi
ANTIC_ROOT=$PREFIX/antic-${ANTIC_VERSION}-${ANTIC_VARIANT}
BUILD_ROOT=${BUILD_ROOT:-/build}
if [ -d ${BUILD_ROOT} ]; then
    mkdir -p ${BUILD_ROOT}
fi
ANTIC_SRC=${BUILD_ROOT}/antic-${ANTIC_VERSION}
ANTIC_BUILD=${BUILD_ROOT}/antic-${ANTIC_VERSION}-${ANTIC_VARIANT}


if [ ! -d ${ANTIC_SRC} ]; then
    git clone https://github.com/wbhart/antic ${ANTIC_SRC}
    cd ${BUILD_ROOT}/antic && git checkout ${ANTIC_COMMIT}
fi
export CMAKE_PREFIX_PATH=$FLINT_ROOT
cmake \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
    -DCMAKE_GENERATOR=Ninja \
    -DCMAKE_C_COMPILER_LAUNCHER=ccache \
    -DCMAKE_CXX_COMPILER_LAUNCHER=ccache \
    -DCMAKE_BUILD_TYPE=${ANTIC_VARIANT^} \
    -DCMAKE_INSTALL_PREFIX=${ANTIC_ROOT} \
    -S ${ANTIC_SRC} \
    -B ${ANTIC_BUILD}
cmake --build ${ANTIC_BUILD}
cmake --install ${ANTIC_BUILD}
cmake --build ${ANTIC_BUILD} --target clean
ln -fs ${ANTIC_BUILD}/compile_commands.json ${ANTIC_ROOT}
