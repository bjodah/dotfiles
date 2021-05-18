#!/bin/bash -xe
wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key|sudo apt-key add -
UBUNTU_CODENAME=$(grep UBUNTU_CODENAME /etc/os-release | cut -d= -f2)
echo "deb http://apt.llvm.org/$UBUNTU_CODENAME/ llvm-toolchain-$UBUNTU_CODENAME-12 main" | sudo tee -a /etc/apt/sources.list
echo "deb-src http://apt.llvm.org/$UBUNTU_CODENAME/ llvm-toolchain-$UBUNTU_CODENAME-12 main" | sudo tee -a /etc/apt/sources.list
sudo apt-get update
sudo apt-get --assume-yes --no-install-recommends install \
    clang-12 libllvm11 libclang-12-dev lldb-12 llvm-12 llvm-12-dev llvm-12-runtime clang-format-12 clang-tidy-12 libomp-12-dev libclang-12-dev
python3 -m pip install --upgrade-strategy=eager --upgrade pip setuptools wheel
python3 -m pip install libclang==12.0.0 && python3 -c "from clang.cindex import Index; Index.create()"
