#!/bin/bash -xe
OPENBLAS_VERSION=${1:-0.3.16}
curl -Ls https://github.com/xianyi/OpenBLAS/releases/download/v${OPENBLAS_VERSION}/OpenBLAS-${OPENBLAS_VERSION}.tar.gz | tar xz -C /build
for VARIANT in debug release; do
    SRC_DIR=/build/OpenBLAS-${OPENBLAS_VERSION}
    BUILD_DIR=/build/openblas-${OPENBLAS_VERSION}-${VARIANT}
    INSTALL_DIR=/opt/openblas-${OPENBLAS_VERSION}-${VARIANT}
    if [ ! -d ${BUILD_DIR} ]; then
        mkdir ${BUILD_DIR}
    fi
    if [ -d "${INSTALL_DIR}" ]; then
        >&2 echo "Install dir already exists: ${INSTALL_DIR} (please delete first)"
        exit 1
    fi
    cd ${BUILD_DIR}
    cmake -G Ninja \
          -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
          -DCMAKE_BUILD_TYPE=${VARIANT^^} \
          -DCMAKE_INSTALL_PREFIX=${INSTALL_DIR} \
          -DBUILD_SHARED_LIBS=ON \
          ${@:2} ${SRC_DIR}
    cmake --build .
    cmake --install .
    cmake --build . --target clean
    ln -sf ${BUILD_DIR}/compile_commands.json ${INSTALL_DIR}/
done
