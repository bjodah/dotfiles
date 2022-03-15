#!/bin/bash -xe
GCC_MAJ=${GCC_MAJ:-12}
export CC=gcc-$GCC_MAJ
export CXX=g++-$GCC_MAJ
#export FC=gfortran-$GCC_MAJ
BOOST_MAJOR=1
BOOST_MINOR=78
BOOST_PATCH=0
VER_P=${BOOST_MAJOR}.${BOOST_MINOR}.${BOOST_PATCH}
VER_U=${BOOST_MAJOR}_${BOOST_MINOR}_${BOOST_PATCH}
BOOST_PREFIX=/opt/boost-${BOOST_MAJOR}.${BOOST_MINOR}.${BOOST_PATCH}
cd /build
curl -Ls https://boostorg.jfrog.io/artifactory/main/release/${VER_P}/source/boost_${VER_U}.tar.bz2 | tar xj
#patch -p1 <$PATCH
cd boost_$VER_U/
ln -s $(which $CXX) g++
ln -s $(which $CC) gcc
export PATH="$(pwd):$PATH"
echo "using gcc : $($CC --version | head -n 1 | cut -d' ' -f4) : $(which $CXX) ; " >> tools/build/src/user-config.jam
./bootstrap.sh --with-toolset=gcc --with-python=python3 --prefix=${BOOST_PREFIX}  #.p
bear ./b2 -j4 install
cp compile_commands.json ${BOOST_PREFIX}/
./b2 --clean-all -n
rm g++ gcc
