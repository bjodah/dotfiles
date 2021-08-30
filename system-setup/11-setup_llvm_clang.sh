#!/bin/bash -xe
MAJOR_VERSION=$1
wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key|sudo apt-key add -
UBUNTU_CODENAME=$(grep UBUNTU_CODENAME /etc/os-release | cut -d= -f2)
APT_LIST_DISTRIBUTION=llvm-toolchain-$UBUNTU_CODENAME
if [[ $MAJOR_VERSION != "" ]]; then
    APT_LIST_DISTRIBUTION=$APT_LIST_DISTRIBUTION-${MAJOR_VERSION}
fi
echo "deb http://apt.llvm.org/$UBUNTU_CODENAME/ $APT_LIST_DISTRIBUTION main" | sudo tee -a /etc/apt/sources.list
echo "deb-src http://apt.llvm.org/$UBUNTU_CODENAME/ $APT_LIST_DISTRIBUTION main" | sudo tee -a /etc/apt/sources.list
sudo apt-get update
sudo apt-get --assume-yes --no-install-recommends install \
    clang-${MAJOR_VERSION} \
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

python3 -m pip install --upgrade-strategy=eager --upgrade pip setuptools wheel
python3 -m pip install libclang==12.0.0 && python3 -c "from clang.cindex import Index; Index.create()"
