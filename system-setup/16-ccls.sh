#!/bin/bash -ex
cd /opt
git clone --depth=1 --recursive https://github.com/MaskRay/ccls
cd ccls 
CC=clang-13 CXX=clang++-13 cmake \
  -H. \
  -Bbuild-rel \
  -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_INSTALL_PREFIX=/usr/local \
  -DBUILD_SHARED_LIBS=ON \
  -DCLANG_RESOURCE_DIR=$(clang-13 -print-resource-dir)
        
cmake --build build-rel
sudo cmake --build build-rel --target install
cmake --build build-rel --target clean


