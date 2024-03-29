#!/bin/bash -eux
LLVM_MAJOR=13  #$(echo $LLVM_ORG_VER | cut -f1 -d.)
export CC=clang-${LLVM_MAJOR} CXX=clang++-${LLVM_MAJOR}
RELEASE_VER=$LLVM_MAJOR.x
if [[ $RELEASE_VER == *.x ]]; then
    LLVM_ORG_VER=release-$RELEASE_VER
else
    LLVM_ORG_VER=llvmorg-$RELEASE_VER
fi
SRC_DIR=/build/llvm-project-${LLVM_ORG_VER}
LIBCXXABI_INCLUDE=$SRC_DIR/libcxxabi/include

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
    BUILD_DIR_LIBCXX=/build/libcxx${LLVM_MAJOR}-${VARIANT}
    INSTALL_DIR_LIBCXX=/opt/libcxx${LLVM_MAJOR}-${VARIANT}
    if [ -d $INSTALL_DIR_LIBCXX ]; then
        >&2 echo "Directory already exists, skipping: $INSTALL_DIR_LIBCXX"
        continue
    fi
    BUILD_DIR_LIBCXXABI=/build/libcxxabi${LLVM_MAJOR}-${VARIANT}
    INSTALL_DIR_LIBCXXABI=/opt/libcxxabi${LLVM_MAJOR}-${VARIANT}
    if [ ! -d $LIBCXXABI_INCLUDE ]; then
        curl -Ls https://github.com/llvm/llvm-project/archive/refs/heads/release/${RELEASE_VER}.tar.gz | tar xz -C /build
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
        -DLLVM_CONFIG_PATH=/usr/bin/llvm-config-${LLVM_MAJOR} \
        -DCMAKE_INSTALL_PREFIX=$INSTALL_DIR_LIBCXX \
        -DLIBCXXABI_LIBCXX_INCLUDES=$INSTALL_DIR_LIBCXX/include/c++/v1 \
        -DLIBCXXABI_LIBCXX_PATH=$SRC_DIR/libcxx \
        $SRC_DIR/libcxxabi
    cmake --build .
    cmake --build . --target install
    cmake --build . --target clean

    cp $LIBCXXABI_INCLUDE/* /opt/libcxx${LLVM_MAJOR}-${VARIANT}/include/
done
