#!/bin/bash -ex
cd /tmp 
git clone --depth=1 --recursive https://github.com/MaskRay/ccls
cd ccls 
CC=clang-11 CXX=clang++-11 cmake \
  -H. \
  -BRelease \
  -DCMAKE_INSTALL_PREFIX=/usr/local \
  -DBUILD_SHARED_LIBS=ON \
  -DCLANG_RESOURCE_DIR=$(clang-11 -print-resource-dir)
        
cmake --build Release 
sudo cmake --build Release --target install 
cd /tmp
rm -r ./ccls/
