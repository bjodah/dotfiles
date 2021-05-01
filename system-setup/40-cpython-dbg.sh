#!/bin/bash -xeu
BUILD_DIR=/build/cpython-3.9-dbg
mkdir -p $BUILD_DIR
cd $BUILD_DIR
if [ ! -e $BUILD_DIR/config.status ]; then
    curl -Ls https://www.github.com/python/cpython/archive/3.9.tar.gz | tar xz -C $BUILD_DIR --strip-components=1 
    CC=gcc-10 CXX=g++-10 CFLAGS="-Og -g -ggdb3" ./configure \
      --without-pymalloc \
      --with-valgrind \
      --with-pydebug \
      --verbose \
      --prefix=/opt/cpython-3.9-dbg \
      --enable-loadable-sqlite-extensions \
      --enable-shared \
      --with-ensurepip=yes \
      LDFLAGS="-Wl,-rpath=/opt/cpython-3.9-dbg/lib"
else
    >&2 echo "config.status present, assuming configured already"
fi
bear make
make install
/opt/cpython-3.9-dbg/bin/python3 -c "import sqlite3, uuid, lzma, bz2" 
/opt/cpython-3.9-dbg/bin/python3 -m pip install --upgrade --upgrade-strategy=eager pip 
/opt/cpython-3.9-dbg/bin/python3 -m pip install --use-feature=2020-resolver cython pytest 
/opt/cpython-3.9-dbg/bin/python3 -m pip install --use-feature=2020-resolver numpy scipy matplotlib plotly tqdm ipywidgets notebook ipympl wheel appdirs
make clean
ln -s $BUILD_DIR/compile_commands.json /opt/cpython-3.9-dbg/
