#!/bin/bash -xe
apt update
apt-get --assume-yes --no-install-recommends install \
        clang-14 llvm-14-dev clang-format-14 clang-tidy-14 clang-tools-14 \
        clangd-14 libclang-14-dev libclang-common-14-dev python3-clang-14 libclang1-14

apt-get clean
rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# Useful packages deferred for now (too heavy)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# gfortran-12 204 MB
# libicu-dev 47.1 MB

# meta packages:
# mingw-w64 ~100-500MB?
# libc6-dev-i386 7435 kB
# g++-11-multilib 6144 B
# gcc-10-multilib 6144 B

# g++-11-multilib gcc-10-multilib libc6-dev-i386 
