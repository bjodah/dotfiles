#!/bin/bash -e
which make
which bear
set -x
CPYTHON_VERSION=${1:-v3.11.0b3}
#https://www.python.org/ftp/python/3.11.0/Python-3.11.0b1.tar.xz
export CC=${CC:-"gcc-12"}
export CXX=${CXX:-"g++-12"}
set -u
set -o pipefail

SRC_DIR="/build/cpython-${CPYTHON_VERSION}"
if [ ! -d $SRC_DIR ]; then
    mkdir -p "${SRC_DIR}"
    curl -Ls https://www.github.com/python/cpython/archive/${CPYTHON_VERSION}.tar.gz | tar xz -C $SRC_DIR --strip-components=1 
fi

for VARIANT in debug release; do
    if [[ $VARIANT == debug ]]; then
        CONFIGURE_FLAGS="--without-pymalloc --with-valgrind --with-pydebug"
        export CFLAGS="-Og -g3 ${CFLAGS:-}"
    elif [[ $VARIANT == release ]]; then
        CONFIGURE_FLAGS=""
        if [[ $(uname -m) == "x86_64" ]]; then
            export CFLAGS="-O3 -march=nehalem -mtune=skylake  ${CFLAGS:-}"
        else
            export CFLAGS="-O3 ${CFLAGS:-}"
        fi
    else
        >&2 echo "Unkown VARIANT: $VARIANT"
        exit 1
    fi
    BUILD_DIR="/build/cpython-${CPYTHON_VERSION#v}-${VARIANT}"
    INSTALL_DIR="/opt/cpython-${CPYTHON_VERSION#v}-${VARIANT}"
    if [[ -e "$BUILD_DIR" ]]; then
        rm -r "$BUILD_DIR"
    fi
    cp -ra $SRC_DIR $BUILD_DIR
    cd $BUILD_DIR
    if [ ! -e $BUILD_DIR/config.status ]; then
        ./configure \
              ${CONFIGURE_FLAGS} \
              --verbose \
              --prefix=${INSTALL_DIR} \
              --enable-loadable-sqlite-extensions \
              --enable-shared \
              --with-ensurepip=yes \
              LDFLAGS="-Wl,-rpath=${INSTALL_DIR}/lib"
    else
        >&2 echo "config.status present, assuming configured already"
    fi
    bear -- make
    make install
    ${INSTALL_DIR}/bin/python3 -c "import sqlite3, uuid, lzma, bz2" 
    ${INSTALL_DIR}/bin/python3 -m pip install --upgrade --upgrade-strategy=eager pip 
    PYTHON=${INSTALL_DIR}/bin/python3 /opt/15-pip-install.sh
    make clean
    ln -s $BUILD_DIR/compile_commands.json /opt/cpython-${CPYTHON_VERSION#v}-${VARIANT}/
done
