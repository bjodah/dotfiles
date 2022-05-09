#!/bin/bash

# Ubuntu 22.04 similar enough to Debian 11?
sed -e s/wine-stable//g -e s/rr//g -e s/linux-tools-common//g \
    ../../containers/bjodahimg22base/env/10-setup_ubuntu_packages.sh >temp.sh
chmod +x temp.sh
./temp.sh


wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | sudo apt-key add -

cat <<EOF | sudo tee /etc/apt/sources.list.d/llvm.list
deb http://apt.llvm.org/bullseye/ llvm-toolchain-bullseye-14 main
deb-src http://apt.llvm.org/bullseye/ llvm-toolchain-bullseye-14 main
EOF

sudo apt-get update
../../containers/bjodahimg22dev/env/110-setup_llvm_clang.sh

sudo apt-get --quiet --assume-yes --no-install-recommends install \
    ccache cmake curl fish gnupg2 libgmp-dev libjson-c-dev libtool-bin \
    libvterm-dev libwebsockets-dev make ninja-build tmux unzip && \
    
../../containers/bjodahimg22base/env/15-pip-install.sh --user

update-alternatives --install /usr/bin/python python /usr/bin/python3 1000
