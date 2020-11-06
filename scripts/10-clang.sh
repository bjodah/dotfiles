#!/bin/bash -xe
wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key|sudo apt-key add -
UBUNTU_CODENAME=$(grep UBUNTU_CODENAME /etc/os-release | cut -d= -f2)
echo "deb http://apt.llvm.org/$UBUNTU_CODENAME/ llvm-toolchain-$UBUNTU_CODENAME-11 main" | sudo tee -a /etc/apt/sources.list
echo "deb-src http://apt.llvm.org/$UBUNTU_CODENAME/ llvm-toolchain-$UBUNTU_CODENAME-11 main" | sudo tee -a /etc/apt/sources.list
sudo apt-get update
sudo apt-get --assume-yes --no-install-recommends install \
    clang-11 libllvm11 libclang-11-dev lldb-11 llvm-11 llvm-11-dev llvm-11-runtime clang-format-11 clang-tidy-11 libomp-11-dev libclang-11-dev
python3 -m pip install --upgrade-strategy=eager --upgrade pip setuptools wheel
python3 -m pip install libclang && python3 -c "from clang.cindex import TypeKind"
