#!/bin/bash -xe

# /usr/lib/$(uname -m)-linux-gnu/libopenblas.so

if [ ! -z "$OPENBLAS_LIBDIR" ]; then
    OPENBLAS_OVERRIDE=1
else
    OPENBLAS_OVERRIDE=0
fi
set -u
SUNDIALS_VERSION=${1:-6.4.1}
SRC_DIR=/build/sundials-${SUNDIALS_VERSION}
export CC=${CC:-"gcc-12"}

if [ ! -d $SRC_DIR ]; then
    SUNDIALS_SRC_URL="https://github.com/llnl/sundials/releases/download/v${SUNDIALS_VERSION}/sundials-${SUNDIALS_VERSION}.tar.gz"
    curl -Ls "${SUNDIALS_SRC_URL}" | tar xz -C /build
fi

for VARIANT in debug release single extended; do
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
    if [[ $VARIANT == extended || $VARIANT == single ]]; then
        CMAKE_ARGS="-DENABLE_KLU=OFF -DSUNDIALS_PRECISION:STRING=$VARIANT"
        if [[ $VARIANT == extended || $VARIANT == single ]]; then
            SUNDIALS_ENABLE_LAPACK="OFF"
        fi
    elif [[ ${SUNDIALS_ENABLE_KLU:-ON} == ON ]]; then
        CMAKE_ARGS="-DENABLE_KLU=ON \
                  -DKLU_INCLUDE_DIR=/usr/include/suitesparse \
                  -DKLU_LIBRARY_DIR=/usr/lib/$(uname -m)-linux-gnu"
    else
        CMAKE_ARGS="-DENABLE_KLU=OFF"
    fi
    SUNDIALS_ENABLE_LAPACK=${SUNDIALS_ENABLE_LAPACK:-ON}
    if [[ $SUNDIALS_ENABLE_LAPACK == "ON" ]]; then
        if [[ $OPENBLAS_OVERRIDE != 1 ]]; then
            if [[ $VARIANT == "debug" ]]; then
                OPENBLAS_LIBDIR=/opt/openblas-0.3.21-${VARIANT}/lib
            else
                OPENBLAS_LIBDIR=/opt/openblas-0.3.21-release/lib
            fi
            if [ ! -d "${OPENBLAS_LIBDIR}" ]; then
                >&2 echo "Not a directory: ${OPENBLAS_LIBDIR}"
                exit 1
            fi
        fi
        export LDFLAGS=-Wl,-rpath-link,${OPENBLAS_LIBDIR}
    fi
      
    if [[ $VARIANT == extended ]]; then
        CMAKE_ARGS='$CMAKE_ARGS -DSUNDIALS_INDEX_SIZE=64'
    else
        if [[ $VARIANT == debug ]]; then
            CFLAGS_DEBUG_DEFAULT="-Os -g3"
            export CFLAGS=${CFLAGS_DEBUG:-"${CFLAGS_DEBUG_DEFAULT}"}
            BUILD_TYPE=${VARIANT^}
            if [[ $SUNDIALS_ENABLE_LAPACK == "ON" ]]; then
	        if [[ -e ${OPENBLAS_LIBDIR}/libopenblas_d.so ]]; then
		    OPENBLAS_SO=${OPENBLAS_LIBDIR}/libopenblas_d.so
	        elif [[ -e ${OPENBLAS_LIBDIR}/libopenblas.so ]]; then
		    OPENBLAS_SO=${OPENBLAS_LIBDIR}/libopenblas.so	
	        fi
            fi
        else
            if [[ $(uname -m) == "x86_64" ]]; then
                CFLAGS_RELEASE_DEFAULT="-O3 -march=nehalem -mtune=skylake"
            else
                CFLAGS_RELEASE_DEFAULT="-O3"
            fi
            export CFLAGS=${CFLAGS_RELEASE:-"${CFLAGS_RELEASE_DEFAULT}"}
            BUILD_TYPE=Release
            if [[ $SUNDIALS_ENABLE_LAPACK == "ON" ]]; then
                OPENBLAS_SO=${OPENBLAS_LIBDIR}/libopenblas.so
            fi
            if [[ $VARIANT == single ]]; then
                CMAKE_ARGS='$CMAKE_ARGS'
            fi
        fi
        if [[ $SUNDIALS_ENABLE_LAPACK == "ON" ]]; then
	    if [[ ! -e $OPENBLAS_SO ]]; then
	        >&2 echo "Could not find openblas shared object with default name"
	        exit 1
	    fi
            CMAKE_ARGS="${CMAKE_ARGS} \
          -DENABLE_LAPACK=${SUNDIALS_ENABLE_LAPACK} \
          -DLAPACK_LIBRARIES=${OPENBLAS_SO}"
            INSTALL_RPATH="${INSTALL_DIR}/lib;${OPENBLAS_LIBDIR}"
        else
            INSTALL_RPATH=${INSTALL_DIR}/lib
        fi
        CMAKE_ARGS="${CMAKE_ARGS} -DSUNDIALS_INDEX_SIZE=${SUNDIALS_INDEX_SIZE:-32}"
    fi
    if [[ ${SUNDIALS_VERSION:0:1} -lt 6 ]] || [[ ${SUNDIALS_VERSION:0:1} -eq 6 && ${SUNDIALS_VERSION:2:3} -lt 3 ]] ; then
        CMAKE_ARGS="${CMAKE_ARGS} -DUSE_GENERIC_MATH=OFF"
    else
        CMAKE_ARGS="${CMAKE_ARGS} -DSUNDIALS_MATH_LIBRARY=m"
    fi
    # USE_GENERIC_MATH below seems to be needed (xref https://github.com/LLNL/sundials/issues/149 )
    
    cmake -G ${CMAKE_GENERATOR:-Ninja} \
          -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
          -DCMAKE_BUILD_TYPE=${BUILD_TYPE} \
          -DCMAKE_INSTALL_PREFIX=${INSTALL_DIR} \
          -DCMAKE_INSTALL_RPATH=$INSTALL_RPATH \
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
    chmod a+r -R ${BUILD_DIR}
    ln -s ${BUILD_DIR}/compile_commands.json ${INSTALL_DIR}/
done
