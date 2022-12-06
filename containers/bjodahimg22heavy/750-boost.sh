#!/bin/bash -xe
GCC_MAJ=${GCC_MAJ:-12}
export CC=gcc-$GCC_MAJ
export CXX=g++-$GCC_MAJ
#export FC=gfortran-$GCC_MAJ
BOOST_MAJOR=${1:-"1"}
BOOST_MINOR=${2:-"81"}
BOOST_PATCH=${3:-"0.beta1"}

VER_P=${BOOST_MAJOR}.${BOOST_MINOR}.${BOOST_PATCH}
VER_U=${BOOST_MAJOR}_${BOOST_MINOR}_${BOOST_PATCH/.beta/_b}
BOOST_PREFIX=/opt/boost-${BOOST_MAJOR}.${BOOST_MINOR}.${BOOST_PATCH}
cd /build
if [[ $BOOST_PATCH == *beta* ]]; then
    URL_BRANCH=beta
    BOOST_ARCH_FOLDER=boost_${BOOST_MAJOR}_${BOOST_MINOR}_${BOOST_PATCH%.*}
else
    URL_BRANCH=release
    BOOST_ARCH_FOLDER=boost_$VER_U/
fi
if [[ ! -e ./$BOOST_ARCH_FOLDER/ ]]; then
    curl -Ls https://boostorg.jfrog.io/artifactory/main/${URL_BRANCH}/${VER_P}/source/boost_${VER_U}.tar.bz2 | tar xj
fi
#patch -p1 <$PATCH
cd $BOOST_ARCH_FOLDER
ln -sf $(which $CXX) g++
ln -sf $(which $CC) gcc
export PATH="$(pwd):$PATH"
echo "using gcc : $($CC --version | head -n 1 | cut -d' ' -f4) : ccache $(which $CXX) ; " >> tools/build/src/user-config.jam
./bootstrap.sh --with-toolset=gcc --with-python=python3 --prefix=${BOOST_PREFIX}  #.p
bear -- ./b2 -j4 install
cp compile_commands.json ${BOOST_PREFIX}/
./b2 --clean
./b2 --clean-all -n
rm g++ gcc
tar c $BOOST_PREFIX | zstd >/work/opt_boost_${VER_U}.tar.zst
