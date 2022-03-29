#!/bin/bash
set -euxo pipefail

export CXX=${CXX:-"g++-12"} CC=${CC:-"gcc-12"}
FLINT_VERSION=${FLINT_VERSION:-"2.8.4"}
BUILD_ROOT=${BUILD_ROOT:-/build}
mkdir -p ${BUILD_ROOT}
curl -Ls https://github.com/wbhart/flint2/archive/refs/tags/v${FLINT_VERSION}.tar.gz | tar xz -C ${BUILD_ROOT}/
cmake \
    -DCMAKE_BUILD_TYPE=Debug \
    -DCMAKE_INSTALL_PREFIX=/opt/flint2-${FLINT_VERSION}-debug \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
    -DCMAKE_GENERATOR=Ninja \
    -S ${BUILD_ROOT}/flint2-${FLINT_VERSION} \
    -B ${BUILD_ROOT}/flint2-${FLINT_VERSION}-debug
cmake --build ${BUILD_ROOT}/flint2-${FLINT_VERSION}-debug
cmake --install ${BUILD_ROOT}/flint2-${FLINT_VERSION}-debug
cmake --build ${BUILD_ROOT}/flint2-${FLINT_VERSION}-debug --target clean
ln -s ${BUILD_ROOT}/flint2-${FLINT_VERSION}-debug/compile_commands.json /opt/flint2-${FLINT_VERSION}-debug


ARB_VERSION=${ARB_VERSION:-"2.22.1"}
curl -Ls https://github.com/fredrik-johansson/arb/archive/refs/tags/${ARB_VERSION}.tar.gz | tar xz -C ${BUILD_ROOT}/
CMAKE_PREFIX_PATH=/opt/flint2-${FLINT_VERSION}-debug/ cmake \
    -DCMAKE_BUILD_TYPE=Debug \
    -DCMAKE_INSTALL_PREFIX=/opt/arb-${ARB_VERSION}-debug \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
    -DCMAKE_GENERATOR=Ninja \
    -S ${BUILD_ROOT}/arb-${ARB_VERSION} \
    -B ${BUILD_ROOT}/arb-${ARB_VERSION}-debug
sed -i '/    FLINT_ASSERT/d' ${BUILD_ROOT}/arb-${ARB_VERSION}/bernoulli/mod_p_harvey.c
cmake --build ${BUILD_ROOT}/arb-${ARB_VERSION}-debug
cmake --install ${BUILD_ROOT}/arb-${ARB_VERSION}-debug
cmake --build ${BUILD_ROOT}/arb-${ARB_VERSION}-debug --target clean
ln -s ${BUILD_ROOT}/arb-${ARB_VERSION}-debug/compile_commands.json /opt/arb-${ARB_VERSION}-debug

CFLAGS="-I/opt/flint2-${FLINT_VERSION}-debug/include -I/opt/flint2-${FLINT_VERSION}-debug/include/flint -I/opt/arb-${ARB_VERSION}-debug/include" \
      LDFLAGS="-Wl,--disable-new-dtags -Wl,-rpath,/opt/arb-${ARB_VERSION}-debug/lib -L/opt/arb-${ARB_VERSION}-debug/lib -Wl,-rpath,/opt/flint2-${FLINT_VERSION}-debug/lib -L/opt/flint2-${FLINT_VERSION}-debug/lib" \
      python3 -m pip install python-flint
