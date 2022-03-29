#!/bin/bash
set -euxo pipefail

export CXX=${CXX:-"g++-12"} CC=${CC:-"gcc-12"}

FLINT_VERSION=${FLINT_VERSION:-"2.8.4"}
ARB_VERSION=${ARB_VERSION:-"2.22.1"}
ANTIC_VERSION=${ANTIC_VERSION:-"git2021dec"}
ANTIC_COMMIT=${ANTIC_COMMIT:-"28473268c628e937b35f0b19e589416217c88cef"}
BUILD_ROOT=${BUILD_ROOT:-"/build"}

git clone https://github.com/wbhart/antic ${BUILD_ROOT}/antic-${ANTIC_VERSION}
cd ${BUILD_ROOT}/antic && git checkout ${ANTIC_COMMIT}
CMAKE_PREFIX_PATH=/opt/flint2-${FLINT_VERSION}-debug/ cmake \
    -DCMAKE_BUILD_TYPE=Debug \
    -DCMAKE_INSTALL_PREFIX=/opt/antic-${ANTIC_VERSION}-debug \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
    -DCMAKE_GENERATOR=Ninja \
    -S ${BUILD_ROOT}/antic-${ANTIC_VERSION} \
    -B ${BUILD_ROOT}/antic-${ANTIC_VERSION}-debug
cmake --build ${BUILD_ROOT}/antic-${ANTIC_VERSION}-debug
cmake --install ${BUILD_ROOT}/antic-${ANTIC_VERSION}-debug
cmake --build ${BUILD_ROOT}/antic-${ANTIC_VERSION}-debug --target clean
ln -s ${BUILD_ROOT}/antic-${ANTIC_VERSION}-debug/compile_commands.json /opt/antic-${ANTIC_VERSION}-debug


CALCIUM_VERSION=${CALCIUM_VERSION:-"git2022feb"}
CALCIUM_COMMIT=${CALCIUM_COMMIT:-"59a61324a9a113e269d646691a59273b7e784d04"}
git clone https://github.com/fredrik-johansson/calcium ${BUILD_ROOT}/calcium-${CALCIUM_VERSION}
cd ${BUILD_ROOT}/calcium-${CALCIUM_VERSION} && git checkout ${CALCIUM_COMMIT}
CALCIUM_PREFIX=/opt/calcium-${CALCIUM_VERSION}-debug
( \
  export LDFLAGS="-Wl,--disable-new-dtags -Wl,-rpath,/opt/calcium-${CALCIUM_VERSION}-debug/lib -L/opt/calcium-${CALCIUM_VERSION}-debug/lib" ; \
  ./configure \
      --prefix=${CALCIUM_PREFIX} \
      --with-gmp=/usr \
      --with-mpfr=/usr \
      --with-flint=/opt/flint2-${FLINT_VERSION}-debug \
      --with-arb=/opt/arb-${ARB_VERSION}-debug \
      --with-antic=/opt/antic-${ANTIC_VERSION}-debug; \
  bear -- make -j4; \
  make install \
)
cd ${BUILD_ROOT}/calcium-${CALCIUM_VERSION}
ln -fs $(realpath compile_commands.json) ${CALCIUM_PREFIX}/
make clean
ln -fs $(realpath pycalcium/pyca.py) $(${PYTHON:-python3} -c 'import site; print(site.getsitepackages()[0])')/pyca.py
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH:-""}:/opt/calcium-${CALCIUM_VERSION}-debug/lib/:/opt/antic-${ANTIC_VERSION}-debug/lib/:/opt/arb-${ARB_VERSION}-debug/lib/:/opt/flint2-${FLINT_VERSION}-debug/lib/ 
if [[ ${CFLAGS:-""} == *fsanitize=address* ]]; then
    LD_PRELOAD=$(${CXX:-c++} --print-file-name libasan.so) ASAN_OPTIONS=detect_leaks=0 ${PYTHON:-python3} -c "import pyca"
else
    ${PYTHON:-python3} -c "import pyca"
fi
