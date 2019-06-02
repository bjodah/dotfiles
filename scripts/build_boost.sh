#!/bin/bash -xe
GCC_MAJ=8
export CC=gcc-$GCC_MAJ
export CXX=g++-$GCC_MAJ
export FC=gfortran-$GCC_MAJ
BOOST_MAJOR=1
BOOST_MINOR=70
BOOST_PATCH=0
VER_P = $BOOST_MAJOR.$BOOST_MINOR.$BOOST_PATCH
VER_U = $BOOST_MAJOR_$BOOST_MINOR_$BOOST_PATCH
cd /dev/shm
curl -Ls https://dl.bintray.com/boostorg/release/$VER_P/source/boost_$VER_U.tar.bz2 | tar xjv
cd boost_$VER_U/
echo "using gcc : 8.3 : $(which $CXX) ; " >> tools/build/src/user-config.jam && ./bootstrap.sh --with-python=python3 --prefix=/opt/boost_$VER_U && ./b2 install
