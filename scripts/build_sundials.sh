#!/bin/bash -xe
SUNDIALS_VERSION=${1:-5.5.0}
SRC_DIR=/build/sundials-${SUNDIALS_VERSION}

if [ ! -d $SRC_DIR ]; then
    curl -Ls https://github.com/llnl/sundials/releases/download/v${SUNDIALS_VERSION}/sundials-${SUNDIALS_VERSION}.tar.gz | tar xz -C /build
fi
for VARIANT in debug release; do
    BUILD_DIR=/build/sundials-${SUNDIALS_VERSION}-${VARIANT}
    INSTALL_DIR=/opt/sundials-${SUNDIALS_VERSION}-${VARIANT}
    if [ ! -d ${BUILD_DIR} ]; then
        mkdir ${BUILD_DIR}
    fi
    cd ${BUILD_DIR}
    OPENBLAS_ROOT=${OPENBLAS_ROOT:-/opt/openblas-0.3.12-${VARIANT}}
    export LDFLAGS=-Wl,-rpath-link,${OPENBLAS_ROOT}/lib
    # if [[ $VARIANT == debug ]]; then
    #     CMAKE_ARGS=
    # elif [[ $VARIANT == release ]]; then
    #     export CFLAGS="-march=native -O3" 
    #     CMAKE_ARGS=
    # else
    #     >&2 echo "Unkown VARIANT: $VARIANT"
    #     exit 1
    # fi
    cmake -G ${CMAKE_GENERATOR:-Ninja} \
          -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
          -DCMAKE_BUILD_TYPE=${VARIANT^^} \
          -DCMAKE_INSTALL_PREFIX=${INSTALL_DIR} \
          -DBUILD_SHARED_LIBS=ON \
          -DBUILD_STATIC_LIBS=OFF \
          -DEXAMPLES_ENABLE_C=ON \
          -DEXAMPLES_INSTALL=ON \
          -DENABLE_LAPACK=ON \
          -DSUNDIALS_INDEX_SIZE=32 \
          -DENABLE_KLU=ON \
          -DLAPACK_LIBRARIES=${OPENBLAS_ROOT}/lib/libopenblas.so \
          -DCMAKE_INSTALL_RPATH="${INSTALL_DIR}/lib;${OPENBLAS_ROOT}/lib" \
          -DCMAKE_BUILD_WITH_INSTALL_RPATH=ON \
          ${CMAKE_ARGS} \
          ${SRC_DIR}
    cmake --build .
    cmake --install .
    ln -s ${BUILD_DIR}/compile_commands.json ${INSTALL_DIR}/
done
