#!/bin/bash
CXX=clang++-15 CC=clang-15 cmake \
    -G Ninja \
    -DCMAKE_BUILD_TYPE=Release \
    -DLLVM_ENABLE_PROJECTS="clang;clang-tools-extra;llvm" \
    -DLLVM_BUILD_TESTS=ON \
    -DLLVM_TARGETS_TO_BUILD=X86 \
    -DLLVM_PARALLEL_LINK_JOBS=1 \
    -DLLVM_USE_LINKER=lld \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
    -DPYTHON_EXECUTABLE=/opt/cpython-3.9/bin/python3.9 \
    -S ../llvm \
    -B .
