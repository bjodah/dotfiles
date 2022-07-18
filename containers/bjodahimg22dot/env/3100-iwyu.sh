#!/bin/bash
set -euxo pipefail
CLANG_VERSION=$1
BUILD_ROOT=${BUILD_ROOT:-"/build"}

git clone --branch clang_${CLANG_VERSION} https://github.com/include-what-you-use/include-what-you-use.git /build/iwyu-${CLANG_VERSION}-src
CXX=clang++-${CLANG_VERSION} CC=clang-${CLANG_VERSION} cmake \
   -S /build/iwyu-${CLANG_VERSION}-src \
   -B /build/iwyu-${CLANG_VERSION}-bld \
   -DCMAKE_BUILD_TYPE=Release \
   -DCMAKE_PREFIX_PATH=/usr/lib/llvm-${CLANG_VERSION}

cmake --build /build/iwyu-${CLANG_VERSION}-bld
cmake --install /build/iwyu-${CLANG_VERSION}-bld
