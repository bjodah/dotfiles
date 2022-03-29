#!/bin/bash
set -euxo pipefail
BUILD_ROOT=${BUILD_ROOT:-/build}
cd $BUILD_ROOT
git clone --depth 1 https://github.com/tsl0922/ttyd.git
cd ttyd
cmake \
    -S . \
    -B ./build/
cmake --build ./build/
cmake --install ./build/
