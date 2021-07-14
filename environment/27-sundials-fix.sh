#!/bin/bash -xe
SUNDIALS_VERSION=${1:-5.7.0}
SRC_DIR=/build/sundials-${SUNDIALS_VERSION}

if [ ! -d $SRC_DIR ]; then
    curl -Ls https://github.com/llnl/sundials/releases/download/v${SUNDIALS_VERSION}/sundials-${SUNDIALS_VERSION}.tar.gz | tar xz -C /build
fi
if [ ! -z "$OPENBLAS_ROOT" ]; then
    OPENBLAS_OVERRIDE=1
fi

for VARIANT in msan single extended; do
    if [[ ${VARIANT} == msan ]]; then
        export CC=${CC:-"clang-12"}
    else
        export CC=${CC:-"gcc-11"}
    fi
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
        if [[ $VARIANT == debug ]]; then
            OPENBLAS_ROOT=/opt/openblas-0.3.16-${VARIANT}
        else
            OPENBLAS_ROOT=/opt/openblas-0.3.16-release
        fi
        if [ ! -d "${OPENBLAS_ROOT}" ]; then
            >&2 echo "Not a directory: ${OPENBLAS_ROOT}"
            exit 1
        fi
    fi
    if [[ $VARIANT == msan ]]; then
        export CFLAGS="-fsanitize=memory -fsanitize-memory-track-origins=2 -fno-omit-frame-pointer -fno-optimize-sibling-calls -O1"
        CMAKE_ARGS='\
                -DENABLE_LAPACK=OFF \
                -DENABLE_KLU=OFF \
                -DSUNDIALS_INDEX_SIZE=32'
    elif [[ $VARIANT == extended ]]; then
        CMAKE_ARGS='\
                -DENABLE_LAPACK=OFF \
                -DENABLE_KLU=OFF \
                -DSUNDIALS_PRECISION:STRING=extended \
                -DSUNDIALS_INDEX_SIZE=64'
    else
        if [[ $VARIANT == debug ]]; then
            BUILD_TYPE=${VARIANT^}
            OPENBLAS_SO=${OPENBLAS_ROOT}/lib/libopenblas_d.so
        else
            export CFLAGS=${CFLAGS_RELEASE:-"-O3"}
            BUILD_TYPE=Release
            OPENBLAS_SO=${OPENBLAS_ROOT}/lib/libopenblas.so
            if [[ $VARIANT == single ]]; then
                CMAKE_ARGS='-DSUNDIALS_PRECISION:STRING=single \
                            -DENABLE_KLU=OFF \'
            else
                CMAKE_ARGS='-DENABLE_KLU=ON'
            fi
        fi
        export LDFLAGS=-Wl,-rpath-link,${OPENBLAS_ROOT}/lib
        CMAKE_ARGS="${CMAKE_ARGS} \
          -DENABLE_LAPACK=ON \
          -DLAPACK_LIBRARIES=${OPENBLAS_SO} \
          -DSUNDIALS_INDEX_SIZE=32 \
          -DKLU_INCLUDE_DIR=/usr/include/suitesparse \
          -DKLU_LIBRARY_DIR=/usr/lib/x86_64-linux-gnu"
    fi
    cmake -G ${CMAKE_GENERATOR:-Ninja} \
          -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
          -DCMAKE_BUILD_TYPE=${BUILD_TYPE} \
          -DCMAKE_INSTALL_PREFIX=${INSTALL_DIR} \
          -DCMAKE_INSTALL_RPATH="${INSTALL_DIR}/lib;${OPENBLAS_ROOT}/lib" \
          -DCMAKE_BUILD_WITH_INSTALL_RPATH=ON \
          -DBUILD_SHARED_LIBS=ON \
          -DBUILD_STATIC_LIBS=OFF \
          -DEXAMPLES_ENABLE_C=ON \
          -DEXAMPLES_INSTALL=ON \
          ${CMAKE_ARGS} \
          ${SRC_DIR}
    cmake --build .
    cmake --install .
    cmake --build . --target clean
    ln -s ${BUILD_DIR}/compile_commands.json ${INSTALL_DIR}/
done
