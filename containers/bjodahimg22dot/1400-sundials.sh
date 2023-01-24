#!/bin/bash

# /usr/lib/$(uname -m)-linux-gnu/libopenblas.so

set -euxo pipefail
PREFIX=$1
if [ ! -d $PREFIX ]; then
    >&2 echo "Non-existent PREFIX (first arg): $PREFIX"
fi
#SUNDIALS_VERSION=${1:-6.5.0}
SUNDIALS_ROOT=$PREFIX/sundials-$SUNDIALS_VERSION-$SUNDIALS_VARIANT
SRC_ROOT=${SRC_ROOT:-"/src"}
BUILD_ROOT=${BUILD_ROOT:-"/build"}
if [ -d ${SRC_ROOT} ]; then
    mkdir -p ${SRC_ROOT}
fi
if [ -d ${BUILD_ROOT} ]; then
    mkdir -p ${BUILD_ROOT}
fi
SUNDIALS_SRC=${SRC_ROOT}/sundials-${SUNDIALS_VERSION}
SUNDIALS_BUILD=${BUILD_ROOT}/sundials-${SUNDIALS_VERSION}-${SUNDIALS_VARIANT}

if [ ! -z "$OPENBLAS_LIBDIR" ]; then
    OPENBLAS_OVERRIDE=1
else
    OPENBLAS_OVERRIDE=0
fi

if [ ! -d $SUNDIALS_SRC ]; then
    SUNDIALS_SRC_URL="https://github.com/llnl/sundials/releases/download/v${SUNDIALS_VERSION}/sundials-${SUNDIALS_VERSION}.tar.gz"
    curl -Ls "${SUNDIALS_SRC_URL}" | tar xz -C $SRC_ROOT
fi

case $SUNDIALS_VARIANT in
    debug)
        break
        ;;
    release)
        break
        ;;
    single)
        break
        ;;
    extended)
        break
        ;;
    msan)
        break
        ;;
    *)
        >&2 echo "Unknown variant: ${SUNDIALS_VARIANT}"
        exit 1
esac

if [[ $SUNDIALS_VARIANT == extended || $SUNDIALS_VARIANT == single ]]; then
    CMAKE_ARGS="-DENABLE_KLU=OFF -DSUNDIALS_PRECISION:STRING=$SUNDIALS_VARIANT"
    SUNDIALS_ENABLE_LAPACK="OFF"
elif [[ $SUNDIALS_VARIANT == msan ]]; then
    CMAKE_ARGS="-DENABLE_KLU=OFF"
    SUNDIALS_INDEX_SIZE=32
    SUNDIALS_ENABLE_LAPACK="OFF"
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
        if [[ $SUNDIALS_VARIANT == "debug" ]]; then
            OPENBLAS_LIBDIR=/opt/openblas-0.3.21-${SUNDIALS_VARIANT}/lib
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

if [[ $SUNDIALS_VARIANT == extended ]]; then
    CMAKE_ARGS='$CMAKE_ARGS -DSUNDIALS_INDEX_SIZE=64'
    BUILD_TYPE=Release
    INSTALL_RPATH=${SUNDIALS_ROOT}/lib
else
    if [[ $SUNDIALS_VARIANT == debug ]]; then
        CFLAGS_DEBUG_DEFAULT="-Os -g3"
        export CFLAGS=${CFLAGS_DEBUG:-"${CFLAGS_DEBUG_DEFAULT}"}
        export CC=${GNU_CC:-gcc}
        BUILD_TYPE=${SUNDIALS_VARIANT^}
        if [[ $SUNDIALS_ENABLE_LAPACK == "ON" ]]; then
	    if [[ -e ${OPENBLAS_LIBDIR}/libopenblas_d.so ]]; then
		OPENBLAS_SO=${OPENBLAS_LIBDIR}/libopenblas_d.so
	    elif [[ -e ${OPENBLAS_LIBDIR}/libopenblas.so ]]; then
		OPENBLAS_SO=${OPENBLAS_LIBDIR}/libopenblas.so	
	    fi
        fi
    elif [[ $SUNDIALS_VARIANT == msan ]]; then
        export CFLAGS="-fsanitize=memory -fsanitize-memory-track-origins=2 -fsanitize-memory-param-retval -fno-omit-frame-pointer -fno-optimize-sibling-calls -O0 -g"
        export CC=${CLANG_CC:-"clang"}
        BUILD_TYPE=Debug
    else
        if [[ $(uname -m) == "x86_64" ]]; then
            CFLAGS_RELEASE_DEFAULT="-O3 -march=nehalem -mtune=skylake"
        else
            CFLAGS_RELEASE_DEFAULT="-O3"
        fi
        export CFLAGS=${CFLAGS_RELEASE:-"${CFLAGS_RELEASE_DEFAULT}"}
        export CC=${GNU_CC:-gcc}
        BUILD_TYPE=Release
        if [[ $SUNDIALS_ENABLE_LAPACK == "ON" ]]; then
            OPENBLAS_SO=${OPENBLAS_LIBDIR}/libopenblas.so
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
        INSTALL_RPATH="${SUNDIALS_ROOT}/lib;${OPENBLAS_LIBDIR}"
    else
        INSTALL_RPATH=${SUNDIALS_ROOT}/lib
    fi
    CMAKE_ARGS="${CMAKE_ARGS} -DSUNDIALS_INDEX_SIZE=${SUNDIALS_INDEX_SIZE:-32}"
fi
if [[ ${SUNDIALS_VERSION:0:1} -lt 6 ]] || [[ ${SUNDIALS_VERSION:0:1} -eq 6 && ${SUNDIALS_VERSION:2:1} -lt 3 ]] ; then
    CMAKE_ARGS="${CMAKE_ARGS} -DUSE_GENERIC_MATH=OFF"
else
    : # CMAKE_ARGS="${CMAKE_ARGS} -DSUNDIALS_MATH_LIBRARY=m"
fi
# USE_GENERIC_MATH below seems to be needed (xref https://github.com/LLNL/sundials/issues/149 )

#-G ${CMAKE_GENERATOR:-Ninja} \
cmake \
      -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
      -DCMAKE_C_COMPILER_LAUNCHER=ccache \
      -DCMAKE_BUILD_TYPE=${BUILD_TYPE} \
      -DCMAKE_INSTALL_PREFIX=${SUNDIALS_ROOT} \
      -DCMAKE_INSTALL_RPATH=$INSTALL_RPATH \
      -DCMAKE_BUILD_WITH_INSTALL_RPATH=ON \
      -DBUILD_SHARED_LIBS=ON \
      -DBUILD_STATIC_LIBS=OFF \
      -DEXAMPLES_ENABLE_C=ON \
      -DEXAMPLES_INSTALL=ON \
      ${CMAKE_ARGS} \
      -S ${SUNDIALS_SRC} \
      -B ${SUNDIALS_BUILD}
cmake --build ${SUNDIALS_BUILD}
cmake --install ${SUNDIALS_BUILD}
cmake --build ${SUNDIALS_BUILD} --target clean
chmod a+r -R ${BUILD_ROOT}
ln -s ${SUNDIALS_BUILD}/compile_commands.json ${SUNDIALS_ROOT}/
