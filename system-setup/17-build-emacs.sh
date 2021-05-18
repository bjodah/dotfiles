#!/bin/bash -e
mkdir -p /build
cd /build
git clone --depth 1 --branch emacs-27 git://github.com/emacs-mirror/emacs
cd emacs
./autogen.sh
CC=gcc-11 CXX=g++-11 ./configure --with-imagemagick --with-modules --with-json --prefix=/usr/local
bear make -j $(nproc)
sudo make install
make clean
#cd /build
#rm -r emacs
