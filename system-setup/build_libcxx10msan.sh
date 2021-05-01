#!/bin/bash
set -euxo pipefail
export CC=clang-10 CXX=clang++-10
export LLVM_ORG_VER=10.0.0-rc4
curl -Ls https://github.com/llvm/llvm-project/archive/llvmorg-${LLVM_ORG_VER}.tar.gz | tar xz -C /tmp 
mkdir /tmp/build_libcxx
cd /tmp/build_libcxx 
cmake -DCMAKE_BUILD_TYPE=Release -DLLVM_USE_SANITIZER=MemoryWithOrigins \
      -DLLVM_CONFIG_PATH=/usr/bin/llvm-config-10 \
      -DCMAKE_INSTALL_PREFIX=/opt/libcxx10_msan /tmp/llvm-project-llvmorg-${LLVM_ORG_VER}/libcxx 
make install 
cd - 
mkdir /tmp/build_libcxxabi 
cd /tmp/build_libcxxabi 
cmake -DCMAKE_BUILD_TYPE=Release -DLLVM_USE_SANITIZER=MemoryWithOrigins \
      -DLIBCXXABI_LIBCXX_INCLUDES=/opt/libcxx10_msan/include/c++/v1 -DLLVM_CONFIG_PATH=/usr/bin/llvm-config-10 -DLIBCXXABI_LIBCXX_PATH=/tmp/llvm-project-llvmorg-${LLVM_ORG_VER}/libcxx \
      -DCMAKE_INSTALL_PREFIX=/opt/libcxx10_msan /tmp/llvm-project-llvmorg-${LLVM_ORG_VER}/libcxxabi 
make install 
cd - 
rm -r /tmp/build_* /tmp/llvm-project*
