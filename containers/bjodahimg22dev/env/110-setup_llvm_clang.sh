#!/bin/bash
set -euxo pipefail
MAJOR_VERSION=$1
sudo apt-get update

# we have already installed: clang-${MAJOR_VERSION}

sudo apt-get --assume-yes --no-install-recommends install \
    clangd-${MAJOR_VERSION} \
    libllvm${MAJOR_VERSION} \
    libclang-${MAJOR_VERSION}-dev \
    lldb-${MAJOR_VERSION} \
    llvm-${MAJOR_VERSION} \
    llvm-${MAJOR_VERSION}-dev \
    llvm-${MAJOR_VERSION}-runtime \
    clang-format-${MAJOR_VERSION} \
    clang-tidy-${MAJOR_VERSION} \
    libomp-${MAJOR_VERSION}-dev \
    libclang-${MAJOR_VERSION}-dev \
    libclang-common-${MAJOR_VERSION}-dev \
    python3-clang-${MAJOR_VERSION}


# Fix for lldb-15 (https://github.com/llvm/llvm-project/issues/55575#issuecomment-1379470228)
cp -ra /usr/lib/llvm-15/lib/python3.10/dist-packages/lldb/* /usr/lib/python3/dist-packages/lldb/
