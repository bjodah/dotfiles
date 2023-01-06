#!/bin/bash -e
which make
which bear
set -x
#CPYTHON_VERSION=${1:-v3.12.0a2}
set -u
set -o pipefail

PREFIX=$1
if [ ! -d $PREFIX ]; then
    >&2 echo "Non-existent PREFIX (first arg): $PREFIX"
fi
CPYTHON_ROOT=$PREFIX/cpython-${CPYTHON_VERSION}-${CPYTHON_VARIANT}

BUILD_ROOT=${BUILD_ROOT:-/build}
if [ -d ${BUILD_ROOT} ]; then
    mkdir -p ${BUILD_ROOT}
fi
CPYTHON_SRC=${BUILD_ROOT}/cpython-${CPYTHON_VERSION}

if [ ! -d $CPYTHON_SRC ]; then
    mkdir -p "${CPYTHON_SRC}"
    curl -Ls https://www.github.com/python/cpython/archive/${CPYTHON_VERSION}.tar.gz | tar xz -C $CPYTHON_SRC --strip-components=1 
fi



if [[ $CPYTHON_VARIANT == debug ]]; then
    CONFIGURE_FLAGS="--without-pymalloc --with-valgrind --with-pydebug"
    export CFLAGS="-Og -g3 ${CFLAGS:-}"
elif [[ $CPYTHON_VARIANT == release ]]; then
    CONFIGURE_FLAGS=""
    if [[ $(uname -m) == "x86_64" ]]; then
        export CFLAGS="-Os -march=nehalem -mtune=skylake  ${CFLAGS:-}"
    else
        export CFLAGS="-Os ${CFLAGS:-}"
    fi
else
    >&2 echo "Unkown VARIANT: $CPYTHON_VARIANT"
    exit 1
fi
CPYTHON_BUILD="${CPYTHON_SRC}-${CPYTHON_VARIANT}"

if [[ -e "$CPYTHON_BUILD" ]]; then
    rm -r "$CPYTHON_BUILD"
fi
cp -ra $CPYTHON_SRC $CPYTHON_BUILD
cd $CPYTHON_BUILD
if [ ! -e $CPYTHON_BUILD/config.status ]; then
    ./configure \
        ${CONFIGURE_FLAGS} \
        --verbose \
        --prefix=${CPYTHON_ROOT} \
        --enable-loadable-sqlite-extensions \
        --enable-shared \
        --with-ensurepip=yes \
        CC="ccache ${CC:-gcc}" \
        CXX="ccache ${CXX:-g++}" \
        LDFLAGS="-Wl,-rpath=${CPYTHON_ROOT}/lib"
else
    >&2 echo "config.status present, assuming configured already"
fi
if [ ! -L $CPYTHON_ROOT/compile_commands.json ]; then
    bear -- make -j $(nproc)
    make install
    ${CPYTHON_ROOT}/bin/python3 -c "import sqlite3, uuid, lzma, bz2" 
    ${CPYTHON_ROOT}/bin/python3 -m pip install --upgrade --upgrade-strategy=eager pip 
    make clean
    ln -s $CPYTHON_BUILD/compile_commands.json $CPYTHON_ROOT/compile_commands.json
fi
