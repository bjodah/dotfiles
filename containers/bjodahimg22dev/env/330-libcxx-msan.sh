#!/bin/bash -eux
export CC=${CC:-clang-15} CXX=${CXX:-clang++-15}
export LLVM_ORG_VER=${LLVM_ORG_VER:-15.0.4}
export LLVM_MAJOR=$(echo $LLVM_ORG_VER | cut -f1 -d.)
BUILD_ROOT=${BUILD_ROOT:-/build}
SRC_DIR=${BUILD_ROOT}/llvm-project-llvmorg-${LLVM_ORG_VER}
INSTALL_ROOT=/opt

for VARIANT in debug release msan; do
    if [[ $VARIANT == debug ]]; then
        CMAKE_ARGS=-DCMAKE_BUILD_TYPE=${VARIANT^^}
    elif [[ $VARIANT == release ]]; then
        CMAKE_ARGS=-DCMAKE_BUILD_TYPE=${VARIANT^^}
    elif [[ $VARIANT == msan ]]; then
        CMAKE_ARGS="-DCMAKE_BUILD_TYPE=Debug -DLLVM_USE_SANITIZER=MemoryWithOrigins"
    else
        >&2 echo "Unhandled variant"
        exit 1        
    fi
    BUILD_DIR_VARIANT=${BUILD_ROOT}/libcxx${LLVM_MAJOR}-${VARIANT}
    INSTALL_DIR_VARIANT=${INSTALL_ROOT}/libcxx${LLVM_MAJOR}-${VARIANT}
    if [ -d $INSTALL_DIR_VARIANT ]; then
        >&2 echo "Directory already exists, skipping: $INSTALL_DIR_VARIANT"
        continue
    fi
    if [ ! -d $SRC_DIR ]; then
        curl -Ls https://github.com/llvm/llvm-project/archive/llvmorg-${LLVM_ORG_VER}.tar.gz | tar xz -C ${BUILD_ROOT}
    fi
    mkdir $BUILD_DIR_VARIANT
    cmake $CMAKE_ARGS \
          -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
          -DCMAKE_INSTALL_PREFIX=$INSTALL_DIR_VARIANT \
          -DLLVM_ENABLE_RUNTIMES="libcxx;libcxxabi;libunwind;compiler-rt" \
          -S $SRC_DIR/runtimes \
          -B $BUILD_DIR_VARIANT

    cmake --build $BUILD_DIR_VARIANT
    cmake --build $BUILD_DIR_VARIANT --target install
    cmake --build $BUILD_DIR_VARIANT --target clean
done
rm -r $SRC_DIR/llvm/test/
rm -r $SRC_DIR/clang/test/
rm -r $SRC_DIR/lldb/test/
ln -s this-folder-was-deleted-in-order-to-save-space $SRC_DIR/llvm/test
ln -s this-folder-was-deleted-in-order-to-save-space $SRC_DIR/clang/test
ln -s this-folder-was-deleted-in-order-to-save-space $SRC_DIR/lldb/test
