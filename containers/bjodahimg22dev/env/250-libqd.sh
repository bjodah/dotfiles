#!/bin/bash -xe
LIBQD_VERSION=${1:-"2.3.22"}
LIBQD_PREFIX=${2:-"/opt/qd-${LIBQD_VERSION}"}
curl -Ls https://www.davidhbailey.com/dhbsoftware/qd-$LIBQD_VERSION.tar.gz | tar xz -C /build
cd /build/qd-$LIBQD_VERSION/
CXX=g++-11 CC=gcc-11 FC=gfortran-11 ./configure --enable-shared --prefix=$LIBQD_PREFIX && 
    bear make
make install && cp /build/qd-$LIBQD_VERSION/compile_commands.json /opt/qd-$LIBQD_VERSION/
