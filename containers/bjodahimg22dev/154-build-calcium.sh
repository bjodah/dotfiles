#!/bin/bash
set -euxo pipefail


PREFIX=$1
if [ ! -d $PREFIX ]; then
    >&2 echo "Non-existent PREFIX (first arg): $PREFIX"
fi
CALCIUM_ROOT=$PREFIX/calcium-${CALCIUM_VERSION}-${CALCIUM_VARIANT}

BUILD_ROOT=${BUILD_ROOT:-/build}
if [ -d ${BUILD_ROOT} ]; then
    mkdir -p ${BUILD_ROOT}
fi
CALCIUM_SRC=${BUILD_ROOT}/calcium-${CALCIUM_VERSION}


if [ ! -d $CALCIUM_SRC ]; then
    git clone https://github.com/fredrik-johansson/calcium ${BUILD_ROOT}/calcium-${CALCIUM_VERSION}
    cd ${BUILD_ROOT}/calcium-${CALCIUM_VERSION} && git checkout ${CALCIUM_COMMIT}
else
    cd ${BUILD_ROOT}/calcium-${CALCIUM_VERSION}
fi


./configure \
    --prefix=${CALCIUM_ROOT} \
    --with-gmp=/usr \
    --with-mpfr=/usr \
    --with-flint=${FLINT_ROOT} \
    --with-arb=${ARB_ROOT} \
    --with-antic=${ANTIC_ROOT}; \
    CC="ccache $CC" \
    LDFLAGS="\
-Wl,--disable-new-dtags -Wl,-rpath,${CALCIUM_ROOT}/lib -L${CALCIUM_ROOT}/lib \
-Wl,--disable-new-dtags -Wl,-rpath,${ANTIC_ROOT}/lib -L${ANTIC_ROOT}/lib \
-Wl,--disable-new-dtags -Wl,-rpath,${ARB_ROOT}/lib -L${ARB_ROOT}/lib \
-Wl,--disable-new-dtags -Wl,-rpath,${FLINT_ROOT}/lib -L${FLINT_ROOT}/lib" \
           bear -- make -j4
    make install
cd ${CALCIUM_ROOT}
ln -fs $(realpath ${BUILD_ROOT}/calcium-${CALCIUM_VERSION}/compile_commands.json) .
cp -ra ${BUILD_ROOT}/calcium-${CALCIUM_VERSION}/pycalcium .
sed -i "s@ctypes.util.find_library('calcium')@\"${CALCIUM_ROOT}/lib/libcalcium.so\"@g" pycalcium/pyca.py
sed -i "s@ctypes.util.find_library('arb')@\"${ARB_ROOT}/lib/libarb.so\"@g" pycalcium/pyca.py
sed -i "s@ctypes.util.find_library('flint')@\"${FLINT_ROOT}/lib/libflint.so\"@g" pycalcium/pyca.py
ln -fs $(realpath pycalcium/pyca.py) $(${PYTHON:-python3} -c 'import site; print(site.getsitepackages()[0])')/pyca.py
if [[ ${CFLAGS:-""} == *fsanitize=address* ]]; then
    LD_PRELOAD=$(${CXX:-c++} --print-file-name libasan.so) ASAN_OPTIONS=detect_leaks=0 ${PYTHON:-python3} -c "import pyca"
else
    ${PYTHON:-python3} -c "import pyca"
fi
