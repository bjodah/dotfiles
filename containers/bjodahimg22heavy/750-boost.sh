#!/bin/bash
set -euxo pipefail

PREFIX=$1
if [ ! -d $PREFIX ]; then
    >&2 echo "Non-existent PREFIX (first arg): $PREFIX"
fi
# BOOST_MAJOR=${1:-"1"}
# BOOST_MINOR=${2:-"81"}
# BOOST_PATCH=${3:-"0.beta1"}
VER_P=${BOOST_MAJOR}.${BOOST_MINOR}.${BOOST_PATCH}
VER_U=${BOOST_MAJOR}_${BOOST_MINOR}_${BOOST_PATCH/.beta/_b}
BOOST_ROOT=$PREFIX/boost-${BOOST_MAJOR}.${BOOST_MINOR}.${BOOST_PATCH}

BUILD_ROOT=${BUILD_ROOT:-/build}
if [ -d ${BUILD_ROOT} ]; then
    mkdir -p ${BUILD_ROOT}
fi

if [[ $BOOST_PATCH == *beta* ]]; then
    URL_BRANCH=beta
    BOOST_ARCH_FOLDER=boost_${BOOST_MAJOR}_${BOOST_MINOR}_${BOOST_PATCH%.*}
else
    URL_BRANCH=release
    BOOST_ARCH_FOLDER=boost_$VER_U/
fi
BOOST_SRC=$BUILD_ROOT/$BOOST_ARCH_FOLDER/
if [[ ! -e $BOOST_SRC ]]; then
    URL_BASE=https://boostorg.jfrog.io/artifactory/main
    BOOST_URL=$URL_BASE/${URL_BRANCH}/${VER_P}/source/boost_${VER_U}.tar.bz2
    curl -Ls $BOOST_URL | tar xj -C $BUILD_ROOT
fi
cd $BOOST_SRC
export CXX="ccache g++"
export CC="ccache gcc"
echo "using gcc : $($CXX --version | head -n 1 | cut -d' ' -f4) : $CXX ; " >> tools/build/src/user-config.jam
./bootstrap.sh --with-toolset=gcc --with-python=python3 --prefix=${BOOST_ROOT}
bear -- ./b2 -j$(nproc) install
ln -s $(realpath ./compile_commands.json) ${BOOST_ROOT}/
./b2 --clean
./b2 --clean-all -n
