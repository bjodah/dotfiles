#!/bin/bash -e
which make
which bear
set -x
CPYTHON_VERSION=${1:-3.10}
export CC=${CC:-"gcc-11"}
export CXX=${CXX:-"g++-11"}
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
        export CFLAGS="-O2 ${CFLAGS:-}"
    else
        >&2 echo "Unkown VARIANT: $VARIANT"
        exit 1
    fi
    BUILD_DIR="/build/cpython-${CPYTHON_VERSION}-${VARIANT}"
    INSTALL_DIR="/opt/cpython-${CPYTHON_VERSION}-${VARIANT}"
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
    bear make
    make install
    ${INSTALL_DIR}/bin/python3 -c "import sqlite3, uuid, lzma, bz2" 
    ${INSTALL_DIR}/bin/python3 -m pip install --upgrade --upgrade-strategy=eager pip 
    #${INSTALL_DIR}/bin/python3 -m pip install cython pytest 
    #${INSTALL_DIR}/bin/python3 -m pip install numpy scipy matplotlib plotly tqdm ipywidgets notebook ipympl wheel appdirs ptvsd scikit-optimize
    PYTHON=${INSTALL_DIR}/bin/python3 /opt/15-pip-install.sh
    make clean
    ln -s $BUILD_DIR/compile_commands.json /opt/cpython-${CPYTHON_VERSION}-${VARIANT}/
done
