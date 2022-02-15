#!/bin/bash
set -euxo pipefail
MAJOR_VERSION=$1
curl -Ls https://apt.llvm.org/llvm-snapshot.gpg.key| apt-key add -
CODENAME=$(grep _CODENAME /etc/os-release | cut -d= -f2)
APT_LIST_DISTRIBUTION=llvm-toolchain-$CODENAME
if [[ $MAJOR_VERSION != "" ]]; then
    APT_LIST_DISTRIBUTION=$APT_LIST_DISTRIBUTION-${MAJOR_VERSION}
fi
echo "deb http://apt.llvm.org/$CODENAME/ $APT_LIST_DISTRIBUTION main" | tee -a /etc/apt/sources.list
echo "deb-src http://apt.llvm.org/$CODENAME/ $APT_LIST_DISTRIBUTION main" | tee -a /etc/apt/sources.list
apt-get update
apt-get --assume-yes --no-install-recommends install \
    clang-${MAJOR_VERSION} \
    clangd-${MAJOR_VERSION} \
    libllvm11 \
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

### Python bindings (https://pypi.org/project/libclang/#history) :
# python3 -m pip install --upgrade-strategy=eager --upgrade pip setuptools wheel
# python3 -m pip install libclang==$MAJOR_VERSION.0.0 && python3 -c "from clang.cindex import Index; Index.create()"
