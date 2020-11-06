#!/bin/bash
mkdir -p /tmp/cpy39/build
cd /tmp/cpy39 
curl -Ls https://www.github.com/python/cpython/archive/3.9.tar.gz | tar xz -C /tmp/cpy39 --strip-components=1 
cp -ra /tmp/cpy39 /tmp/cpy39-dbg 
cd /tmp/cpy39-dbg/build 
CC=gcc-10 CXX=g++-10 CFLAGS="-Og -g -ggdb3" .././configure \
  --without-pymalloc \
  --with-valgrind \
  --with-pydebug \
  --verbose \
  --prefix=/opt/cpython-3.9-dbg \
  --enable-loadable-sqlite-extensions \
  --enable-shared \
  --with-ensurepip=yes \
  LDFLAGS="-Wl,-rpath=/opt/cpython-3.9-dbg/lib" \
  
  make install 
/opt/cpython-3.9-dbg/bin/python3 -c "import sqlite3, uuid, lzma, bz2" 
/opt/cpython-3.9-dbg/bin/python3 -m pip install --upgrade --upgrade-strategy=eager pip 
/opt/cpython-3.9-dbg/bin/python3 -m pip install --use-feature=2020-resolver cython pytest 
/opt/cpython-3.9-dbg/bin/python3 -m pip install --use-feature=2020-resolver numpy scipy matplotlib plotly tqdm ipywidgets notebook ipympl wheel appdirs 
