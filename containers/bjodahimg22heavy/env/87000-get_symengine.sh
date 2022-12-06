#!/bin/bash -eu
#
# Example usage:
#
#  $ CXXFLAGS="-O3 -funroll-loops" ./get_symengine.sh v0.4.0 /opt/symengine-0.4.0 Release -DWITH_TCMALLOC=ON
#
TAG=$1
if [[ "$TAG" == v* ]]; then
    FNAME=symengine-${TAG:1}.tar.gz
else
    FNAME=symengine-${TAG}.tar.gz
fi
if [ ! -e $FNAME ]; then
    if [[ "$TAG" == v* ]]; then
        URL=https://github.com/symengine/symengine/releases/download/$TAG/symengine-${TAG:1}.tar.gz
    else
        URL=https://github.com/symengine/symengine/archive/$TAG.tar.gz
    fi
    curl -Ls $URL -o $FNAME
fi
SRCDIR="$PWD/${FNAME%.tar.gz}"
if [ ! -d $SRCDIR ]; then
    tar xzf $FNAME
    SRCDIR=$(echo "$SRCDIR"*/)
    if [ ! -d "$SRCDIR" ]; then
        2>&1 echo "Expected a foldername of extracted file: $SRCDIR"
        exit 1
    fi
fi
TMP_BLD_DIR=/build/$(basename $2) # $(mktemp -d -p /build/ $(basename $2)XXX)
set -x
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -G Ninja -DBUILD_SHARED_LIBS=ON -DBUILD_TESTS=ON -DBUILD_BENCHMARKS=OFF -DCMAKE_INSTALL_PREFIX=$2 -DCMAKE_BUILD_TYPE=$3 "${@:4}" -S "$SRCDIR" -B $TMP_BLD_DIR
cmake --build $TMP_BLD_DIR
ctest --output-on-failure
cmake --install $TMP_BLD_DIR
cmake --build $TMP_BLD_DIR --target clean
ln -s $TMP_BLD_DIR/compile_commands.json $2
rm -r $TMP_BLD_DIR/
