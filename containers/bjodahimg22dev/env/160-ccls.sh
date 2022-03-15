#!/bin/bash -ex
cd /opt
git clone --depth=1 --recursive https://github.com/MaskRay/ccls
cd ccls
export CXXFLAGS="-O3 -pipe -march=nehalem -mtune=skylake -fomit-frame-pointer"
CC=clang-14 CXX=clang++-14 cmake \
  -H. \
  -Bbuild-rel \
  -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_INSTALL_PREFIX=/usr/local \
  -DBUILD_SHARED_LIBS=ON \
  -DCLANG_RESOURCE_DIR=$(clang-14 -print-resource-dir)
        
cmake --build build-rel
sudo cmake --build build-rel --target install
cmake --build build-rel --target clean
