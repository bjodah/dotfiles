#!/bin/bash -eu
branch="$1"
SRCROOT=~/rovc/cpython
cd $SRCROOT
git checkout "$branch"
git pull
cd /build
WORKDIR="/build/cpython-$branch"
if [ -d "$WORKDIR" ]; then
    find "$WORKDIR" -maxdepth 1 -mindepth 1 -exec rm -r {} \;
else
    mkdir "$WORKDIR"
fi
cd "$WORKDIR"

if [ -f $SRCROOT/configure ]; then
    PREFIX="/opt/cpython-$branch"
    CC=gcc-8 CXX=g++-8 FC=gfortran-8 CFLAGS="-O2 -march=native" $SRCROOT/configure --prefix="$PREFIX" --enable-loadable-sqlite-extensions --enable-shared --with-ensurepip=yes LDFLAGS=-Wl,-rpath="$PREFIX/lib"
    make
    if [ -d "$PREFIX" ]; then
        find "$PREFIX" -maxdepth 1 -mindepth 1 -exec rm -r {} \;
    fi
    ./python -c "import sqlite3, uuid, lzma, bz2" && make install
else
    >&2 echo "No configure script found"
fi
"$PREFIX/bin/python3" -m pip install --upgrade --upgrade-strategy=eager pip
PREFIX="$PREFIX" "$PREFIX"/bin/python3 -c 'import os; print("SUCCESS! Add to $PATH: %s/bin" % os.environ["PREFIX"])'
