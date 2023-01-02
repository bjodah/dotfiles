#!/bin/bash
set -euxo pipefail
PREFIX=$1

export LLVM_MAJOR=$(echo $LLVM_ORG_VER | cut -f1 -d.)
BUILD_ROOT=${BUILD_ROOT:-/build}
SRC_DIR=${BUILD_ROOT}/llvm-project-llvmorg-${LLVM_ORG_VER}


if [[ $LIBCXX_VARIANT == debug ]]; then
    CMAKE_ARGS=-DCMAKE_BUILD_TYPE=${LIBCXX_VARIANT^^}
elif [[ $LIBCXX_VARIANT == release ]]; then
    CMAKE_ARGS=-DCMAKE_BUILD_TYPE=${LIBCXX_VARIANT^^}
elif [[ $LIBCXX_VARIANT == msan ]]; then
    CMAKE_ARGS="-DCMAKE_BUILD_TYPE=Debug -DLLVM_USE_SANITIZER=MemoryWithOrigins"
else
    >&2 echo "Unhandled variant"
    exit 1        
fi
BUILD_DIR_VARIANT=${BUILD_ROOT}/libcxx${LLVM_MAJOR}-${LIBCXX_VARIANT}
INSTALL_DIR_VARIANT=${PREFIX}/libcxx${LLVM_MAJOR}-${LIBCXX_VARIANT}
if [ -d $INSTALL_DIR_VARIANT ]; then
    >&2 echo "Directory already exists, skipping: $INSTALL_DIR_VARIANT"
    continue
fi
if [ ! -d $SRC_DIR ]; then
    curl -Ls https://github.com/llvm/llvm-project/archive/llvmorg-${LLVM_ORG_VER}.tar.gz | tar xz -C ${BUILD_ROOT}
fi
mkdir $BUILD_DIR_VARIANT
export CCACHE_CPP2=true
cmake $CMAKE_ARGS \
      -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
      -DCMAKE_GENERATOR=Ninja \
      -DCMAKE_C_COMPILER_LAUNCHER=ccache \
      -DCMAKE_CXX_COMPILER_LAUNCHER=ccache \
      -DCMAKE_INSTALL_PREFIX=$INSTALL_DIR_VARIANT \
      -DLLVM_ENABLE_RUNTIMES="libcxx;libcxxabi;libunwind;compiler-rt" \
      -S $SRC_DIR/runtimes \
      -B $BUILD_DIR_VARIANT

cmake --build $BUILD_DIR_VARIANT
cmake --build $BUILD_DIR_VARIANT --target install
cmake --build $BUILD_DIR_VARIANT --target clean

# rm -r $SRC_DIR/llvm/test/
# rm -r $SRC_DIR/clang/test/
# rm -r $SRC_DIR/lldb/test/
# ln -s this-folder-was-deleted-in-order-to-save-space $SRC_DIR/llvm/test
# ln -s this-folder-was-deleted-in-order-to-save-space $SRC_DIR/clang/test
# ln -s this-folder-was-deleted-in-order-to-save-space $SRC_DIR/lldb/test
