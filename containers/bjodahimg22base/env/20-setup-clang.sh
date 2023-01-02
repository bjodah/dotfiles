#!/bin/bash
set -euxo pipefail
MAJOR_VERSION=$1
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
    clang-${MAJOR_VERSION}

### Python bindings (https://pypi.org/project/libclang/#history) :
# python3 -m pip install --upgrade-strategy=eager --upgrade pip setuptools wheel
# python3 -m pip install libclang==$MAJOR_VERSION.0.0 && python3 -c "from clang.cindex import Index; Index.create()"
