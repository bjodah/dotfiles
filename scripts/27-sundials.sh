#!/bin/bash -xe
SUNDIALS_VERSION=${1:-5.5.0}
SRC_DIR=/build/sundials-${SUNDIALS_VERSION}

if [ ! -d $SRC_DIR ]; then
    curl -Ls https://github.com/llnl/sundials/releases/download/v${SUNDIALS_VERSION}/sundials-${SUNDIALS_VERSION}.tar.gz | tar xz -C /build
fi
if [ ! -z "$OPENBLAS_ROOT" ]; then
    OPENBLAS_OVERRIDE=1
fi

for VARIANT in debug release; do
    BUILD_DIR=/build/sundials-${SUNDIALS_VERSION}-${VARIANT}
    INSTALL_DIR=/opt/sundials-${SUNDIALS_VERSION}-${VARIANT}
    if [ -d "${INSTALL_DIR}" ]; then
        >&2 echo "Install dir exists, skipping: ${INSTALL_DIR}"
        continue
    fi
    if [ ! -d ${BUILD_DIR} ]; then
        mkdir ${BUILD_DIR}
    fi
    cd ${BUILD_DIR}
    if [[ $OPENBLAS_OVERRIDE != 1 ]]; then
        OPENBLAS_ROOT=/opt/openblas-0.3.12-${VARIANT}
    fi
    export LDFLAGS=-Wl,-rpath-link,${OPENBLAS_ROOT}/lib
    if [[ $VARIANT == debug ]]; then
        OPENBLAS_SO=${OPENBLAS_ROOT}/lib/libopenblas_d.so
    elif [[ $VARIANT == release ]]; then
        OPENBLAS_SO=${OPENBLAS_ROOT}/lib/libopenblas.so
        export CFLAGS="-march=native -O3" 
    else
        >&2 echo "Unkown VARIANT: $VARIANT"
        exit 1
    fi
    cmake -G ${CMAKE_GENERATOR:-Ninja} \
          -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
          -DCMAKE_BUILD_TYPE=${VARIANT^^} \
          -DCMAKE_INSTALL_PREFIX=${INSTALL_DIR} \
          -DCMAKE_INSTALL_RPATH="${INSTALL_DIR}/lib;${OPENBLAS_ROOT}/lib" \
          -DCMAKE_BUILD_WITH_INSTALL_RPATH=ON \
          -DBUILD_SHARED_LIBS=ON \
          -DBUILD_STATIC_LIBS=OFF \
          -DEXAMPLES_ENABLE_C=ON \
          -DEXAMPLES_INSTALL=ON \
          -DENABLE_LAPACK=ON \
          -DLAPACK_LIBRARIES=${OPENBLAS_SO} \
          -DSUNDIALS_INDEX_SIZE=32 \
          -DENABLE_KLU=ON \
          -DKLU_INCLUDE_DIR=/usr/include/suitesparse \
          -DKLU_LIBRARY_DIR=/usr/lib/x86_64-linux-gnu \
          ${CMAKE_ARGS} \
          ${SRC_DIR}
    cmake --build .
    cmake --install .
    ln -s ${BUILD_DIR}/compile_commands.json ${INSTALL_DIR}/
done
