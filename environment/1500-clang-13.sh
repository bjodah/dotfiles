#!/bin/bash
UBUNTU_CODENAME=$(grep UBUNTU_CODENAME /etc/os-release | cut -d= -f2)
APT_LIST_DISTRIBUTION=llvm-toolchain-$UBUNTU_CODENAME
echo "deb http://apt.llvm.org/$UBUNTU_CODENAME/ $APT_LIST_DISTRIBUTION main" | sudo tee -a /etc/apt/sources.list
echo "deb-src http://apt.llvm.org/$UBUNTU_CODENAME/l $APT_LIST_DISTRIBUTION main" | sudo tee -a /etc/apt/sources.list
sudo apt-get update
MAJOR_VERSION=13
sudo apt-get --assume-yes --no-install-recommends install \
    clang-${MAJOR_VERSION} \
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
    libclang-common-${MAJOR_VERSION}-dev
