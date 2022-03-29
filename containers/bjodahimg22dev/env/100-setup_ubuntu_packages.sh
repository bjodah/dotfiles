#!/bin/bash -xe
sudo apt-get --assume-yes --no-install-recommends install \
     automake autoconf binutils-dev bison ccache coinor-libipopt-dev cppcheck \
     doxygen dpkg-dev expect fd-find ffmpeg flex fonts-dejavu \
     fonts-humor-sans freeglut3-dev fish fzf g++-12 gcc-12 \
     gcovr gdb lcov \
     libacl1-dev libatomic1 libbz2-dev \
     libc-dev libeigen3-dev libfontconfig1 libfreetype6-dev libgccjit0 libgccjit-12-dev libgdbm-dev \
     libgpm-dev libgif-dev libgit2-dev libgl1-mesa-dev libglfw3-dev libgmp-dev \
     libgnutls28-dev libgoogle-perftools-dev libgraphviz-dev libgsl-dev libgtk-3-dev \
     libjansson-dev libjansson4 libjbig-dev libjpeg-dev libjson-c-dev \
     liblapack-dev liblcms2-dev liblockfile-dev liblzma-dev libm17n-dev libmagick++-dev libmpfr-dev libncurses5-dev \
     libopenmpi-dev libotf-dev libpng-dev libreadline6-dev librsvg2-dev libsdl2-dev libsdl2-gfx-dev \
     libsdl2-image-dev libsdl2-mixer-dev libsdl2-net-dev libsdl2-ttf-dev \
     libsqlite3-dev libssl-dev libsuitesparse-dev libsystemd-dev libtiff-dev libtool libtool-bin \
     libwebp-dev libwebsockets-dev libx11-dev libxml2-dev libxpm-dev libxrender1 libxslt1-dev libyaml-dev \
     libzmq3-dev libzstd-dev lzma-dev \
     ninja-build petsc-dev \
     python3-dev rr tk-dev trilinos-dev valgrind xorg-dev xutils-dev zlib1g-dev linux-tools-common \
     libopenblas-openmp-dev

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
