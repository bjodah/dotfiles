#!/bin/bash
set -euxo pipefail

PREFIX=$1
if [ ! -d $PREFIX ]; then
    >&2 echo "Non-existent PREFIX (first arg): $PREFIX"
fi
SYMENGINE_ROOT=$PREFIX/symengine-${SYMENGINE_VERSION#v}-${SYMENGINE_VARIANT}
BUILD_ROOT=${BUILD_ROOT:-/build}
if [ -d ${BUILD_ROOT} ]; then
    mkdir -p ${BUILD_ROOT}
fi
if [ -z $SYMENGINE_COMMIT ]; then
    SYMENGINE_SRC=${BUILD_ROOT}/symengine-${SYMENGINE_VERSION#v}
else
    SYMENGINE_SRC=${BUILD_ROOT}/symengine-${SYMENGINE_COMMIT}
fi
SYMENGINE_BUILD=${SYMENGINE_SRC}-${SYMENGINE_VARIANT}


if [ ! -d ${SYMENGINE_SRC} ]; then
    if [[ "$SYMENGINE_VERSION" == v* ]]; then
        URL=https://github.com/symengine/symengine/releases/download/$SYMENGINE_VERSION/symengine-${SYMENGINE_VERSION:1}.tar.gz
    else
        URL=https://github.com/symengine/symengine/archive/$SYMENGINE_VERSION.tar.gz
    fi
    curl -Ls $URL | tar xz -C $BUILD_ROOT
fi

if [ ! -d $SYMENGINE_SRC ]; then
    2>&1 echo "Expected a foldername of extracted file: $SYMENGINE_SRC"
    exit 1
fi

case $SYMENGINE_VARIANT in
    release)
        export CXXFLAGS="-std=c++17"
        export CMAKE_ARGS="-DCMAKE_BUILD_TYPE=Release -DWITH_COTIRE=OFF -DWITH_BFD=OFF -DWITH_LLVM=ON  -DINTEGER_CLASS=gmp -DWITH_SYMENGINE_RCP=ON"
        ;;
    debug)
        export CXXFLAGS="-Og -g -ggdb3 -std=c++17 -fsized-deallocation"
        export CMAKE_ARGS="-DCMAKE_BUILD_TYPE=Debug -DWITH_COTIRE=OFF -DWITH_BFD=OFF -DWITH_LLVM=OFF -DINTEGER_CLASS=boostmp"
        export CMAKE_PREFIX_PATH="/$OPT_FOLDER/$BOOST_DIR"
        ;;
    glibcxxdbg)
        export CXXFLAGS="-Og -g -ggdb3 -std=c++17 -D_GLIBCXX_DEBUG -D_GLIBCXX_DEBUG_PEDANTIC -fsized-deallocation"
        export CMAKE_ARGS="-DCMAKE_BUILD_TYPE=Debug -DWITH_COTIRE=OFF -DWITH_BFD=OFF -DWITH_LLVM=OFF -DINTEGER_CLASS=boostmp"
        export CMAKE_PREFIX_PATH="/$OPT_FOLDER/$BOOST_DIR"
        ;;
    asan)
        export CXXFLAGS="-std=c++17 -fsanitize=address -Og -glldb -DHAVE_GCC_ABI_DEMANGLE=no"
        export LDFLAGS="-fsanitize=address" 
        export CMAKE_PREFIX_PATH="/$OPT_FOLDER/$BOOST_DIR"
        export CMAKE_ARGS="-DCMAKE_BUILD_TYPE=Debug -DWITH_COTIRE=OFF -DWITH_BFD=OFF -DWITH_LLVM=ON -DINTEGER_CLASS=boostmp -DWITH_SYMENGINE_RCP=ON"
        export CC=clang
        export CXX=clang++
        ;;
    msan)
        export CXXFLAGS="-std=c++17 -fsanitize=memory -fsanitize-memory-track-origins=2 -fsanitize-memory-param-retval -stdlib=libc++ -I/opt/libcxx15-msan/include -I/opt/libcxx15-msan/include/c++/v1 -fno-omit-frame-pointer -fno-optimize-sibling-calls -O1 -glldb -DHAVE_GCC_ABI_DEMANGLE=no"
        export LDFLAGS="-fsanitize=memory -fsanitize-memory-track-origins=2 -Wl,-rpath,/opt/libcxx15-msan/lib -L/opt/libcxx15-msan/lib -lc++abi" 
        export CMAKE_PREFIX_PATH="/$OPT_FOLDER/$BOOST_DIR"
        export CMAKE_ARGS="-DCMAKE_BUILD_TYPE=Debug -DWITH_COTIRE=OFF -DWITH_BFD=OFF -DWITH_LLVM=OFF -DINTEGER_CLASS=boostmp -DWITH_SYMENGINE_RCP=ON"
        export CC=clang-15
        export CXX=clang++-15
        ;;
    tcmalloc)
        export CXXFLAGS="-std=c++17" CC=clang-15 CXX=clang++-15
        export CMAKE_ARGS="-DCMAKE_BUILD_TYPE=Release -DWITH_COTIRE=OFF -DWITH_BFD=OFF -DWITH_LLVM=ON -DWITH_TCMALLOC=ON"
        export CMAKE_PREFIX_PATH="/opt/boost-1.81.0.beta1:$CMAKE_PREFIX_PATH"
        ;; 
    *)
        >&2 echo "Unhandled case: $SYMENGINE_VARIANT"
        exit 1
esac
cmake \
    -G Ninja \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
    -DCMAKE_C_COMPILER_LAUNCHER=ccache \
    -DCMAKE_CXX_COMPILER_LAUNCHER=ccache \
    -DBUILD_SHARED_LIBS=ON \
    -DBUILD_TESTS=ON \
    -DBUILD_BENCHMARKS=OFF \
    -DCMAKE_INSTALL_PREFIX=$SYMENGINE_ROOT \
    ${CMAKE_ARGS} \
    -S $SYMENGINE_SRC \
    -B $SYMENGINE_BUILD
cmake --build $SYMENGINE_BUILD
( cd $SYMENGINE_BUILD; ctest --output-on-failure )
cmake --install $SYMENGINE_BUILD
cmake --build $SYMENGINE_BUILD --target clean
ln -s $SYMENGINE_BUILD/compile_commands.json $SYMENGINE_ROOT

