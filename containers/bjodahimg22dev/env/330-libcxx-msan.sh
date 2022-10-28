#!/bin/bash -eux
export CC=${CC:-clang-15} CXX=${CXX:-clang++-15}
export LLVM_ORG_VER=${LLVM_ORG_VER:-15.0.3}
export LLVM_MAJOR=$(echo $LLVM_ORG_VER | cut -f1 -d.)
SRC_DIR=/build/llvm-project-llvmorg-${LLVM_ORG_VER}
LIBCXXABI_INCLUDE=$SRC_DIR/libcxxabi/include
BUILD_ROOT=${BUILD_ROOT:-/build}
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
    BUILD_DIR_LIBCXX=${BUILD_ROOT}/libcxx${LLVM_MAJOR}-${VARIANT}
    INSTALL_DIR_LIBCXX=${INSTALL_ROOT}/libcxx${LLVM_MAJOR}-${VARIANT}
    if [ -d $INSTALL_DIR_LIBCXX ]; then
        >&2 echo "Directory already exists, skipping: $INSTALL_DIR_LIBCXX"
        continue
    fi
    BUILD_DIR_LIBCXXABI=${BUILD_ROOT}/libcxxabi${LLVM_MAJOR}-${VARIANT}
    INSTALL_DIR_LIBCXXABI=${INSTALL_ROOT}/libcxxabi${LLVM_MAJOR}-${VARIANT}
    if [ ! -d $LIBCXXABI_INCLUDE ]; then
        curl -Ls https://github.com/llvm/llvm-project/archive/llvmorg-${LLVM_ORG_VER}.tar.gz | tar xz -C ${BUILD_ROOT}
    fi
    mkdir $BUILD_DIR_LIBCXX
    cd $BUILD_DIR_LIBCXX
    cmake $CMAKE_ARGS \
          -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
          -DCMAKE_INSTALL_PREFIX=$INSTALL_DIR_LIBCXX \
          $SRC_DIR/libcxx
          #-DLLVM_CONFIG_PATH=/usr/bin/llvm-config-$LLVM_MAJOR \
    cmake --build .
    cmake --build . --target install
    mkdir $BUILD_DIR_LIBCXXABI
    cd $BUILD_DIR_LIBCXXABI
    cmake \
        $CMAKE_ARGS \
        -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
        -DCMAKE_MODULE_PATH=$SRC_DIR/libcxx/cmake/Modules \
        -DCMAKE_INSTALL_PREFIX=$INSTALL_DIR_LIBCXX \
        -DLIBCXXABI_LIBCXX_INCLUDES=$INSTALL_DIR_LIBCXX/include/c++/v1 \
        -DLIBCXXABI_LIBCXX_PATH=$SRC_DIR/libcxx \
        $SRC_DIR/libcxxabi
        #-DLLVM_CONFIG_PATH=/usr/bin/llvm-config-${LLVM_MAJOR} \

    cmake --build .
    cmake --build . --target install
    cmake --build . --target clean

    cp $LIBCXXABI_INCLUDE/* ${INSTALL_ROOT}/libcxx${LLVM_MAJOR}-${VARIANT}/include/

    cd -
    cmake --build . --target clean
done
rm -r $SRC_DIR/llvm/test/
rm -r $SRC_DIR/clang/test/
rm -r $SRC_DIR/lldb/test/
ln -s this-folder-was-deleted-in-order-to-save-space $SRC_DIR/llvm/test
ln -s this-folder-was-deleted-in-order-to-save-space $SRC_DIR/clang/test
ln -s this-folder-was-deleted-in-order-to-save-space $SRC_DIR/lldb/test
