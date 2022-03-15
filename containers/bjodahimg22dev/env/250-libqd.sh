#!/bin/bash -xe
LIBQD_VERSION=${1:-"2.3.23"}
LIBQD_PREFIX=${2:-"/opt/qd-${LIBQD_VERSION}"}
mkdir -p /build
curl -Ls https://www.davidhbailey.com/dhbsoftware/qd-$LIBQD_VERSION.tar.gz | tar xz -C /build
cd /build/qd-$LIBQD_VERSION/
#FC=gfortran-12
CXX=g++-12 CC=gcc-12 ./configure --enable-shared --prefix=$LIBQD_PREFIX
bear -- make
make install
cp /build/qd-$LIBQD_VERSION/compile_commands.json $LIBQD_PREFIX/
