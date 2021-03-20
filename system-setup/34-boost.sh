#!/bin/bash -xe
GCC_MAJ=${GCC_MAJ:-10}
export CC=gcc-$GCC_MAJ
export CXX=g++-$GCC_MAJ
export FC=gfortran-$GCC_MAJ
BOOST_MAJOR=1
BOOST_MINOR=75
BOOST_PATCH=0
VER_P=${BOOST_MAJOR}.${BOOST_MINOR}.${BOOST_PATCH}
VER_U=${BOOST_MAJOR}_${BOOST_MINOR}_${BOOST_PATCH}
PATCH=$(realpath $(dirname $BASH_SOURCE))/boost175-rational-gh41.patch
cd /build
curl -Ls https://dl.bintray.com/boostorg/release/${VER_P}/source/boost_${VER_U}.tar.bz2 | tar xjv
patch -p1 <$PATCH
cd boost_$VER_U/
echo "using gcc : $($CC --version | head -n 1 | cut -d' ' -f4) : $(which $CXX) ; " >> tools/build/src/user-config.jam
./bootstrap.sh --with-toolset=gcc --with-python=python3 --prefix=/opt/boost-${BOOST_MAJOR}.${BOOST_MINOR}.p
bear ./b2 -j4 install
./b2 --clean-all -n
