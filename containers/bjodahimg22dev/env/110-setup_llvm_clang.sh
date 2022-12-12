#!/bin/bash
set -euxo pipefail
MAJOR_VERSION=$1
if [[ ${USE_APT_GET_FOR_CLANG_INSTALL:-0} == 1 ]]; then
    apt update
    apt-get --assume-yes --no-install-recommends install \
            clang-14 llvm-14-dev clang-format-14 clang-tidy-14 clang-tools-14 \
            clangd-14 libclang-14-dev libclang-common-14-dev python3-clang-14 libclang1-14

    apt-get clean
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
    return
fi
wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key|sudo apt-key add -
CODENAME=$(grep _CODENAME /etc/os-release | head -n 1 | cut -d= -f2 | tr -d '\n')
APT_LIST_DISTRIBUTION=llvm-toolchain-$CODENAME
if [[ $MAJOR_VERSION != "" ]]; then
    APT_LIST_DISTRIBUTION=$APT_LIST_DISTRIBUTION-${MAJOR_VERSION}
fi
echo "deb http://apt.llvm.org/$CODENAME/ $APT_LIST_DISTRIBUTION main" | sudo tee -a /etc/apt/sources.list
echo "deb-src http://apt.llvm.org/$CODENAME/ $APT_LIST_DISTRIBUTION main" | sudo tee -a /etc/apt/sources.list
sudo apt-get update
sudo apt-get --assume-yes --no-install-recommends install \
    clang-${MAJOR_VERSION} \
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

### Python bindings (https://pypi.org/project/libclang/#history) :
# python3 -m pip install --upgrade-strategy=eager --upgrade pip setuptools wheel
# python3 -m pip install libclang==$MAJOR_VERSION.0.0 && python3 -c "from clang.cindex import Index; Index.create()"
