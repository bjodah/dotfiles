#!/bin/bash -xe
sudo apt-get --assume-yes --no-install-recommends install \
     automake bear biber binutils-dev bison cmake coinor-libipopt-dev cppcheck \
     curl dot2tex doxygen dpkg-dev dvipng expect fd-find ffmpeg flex fonts-dejavu \
     fonts-humor-sans freeglut3-dev g++-10 g++-10-multilib gcc-10 \
     gcc-10-multilib gcovr gdb gfortran-10 git gnupg2 gnuplot graphviz \
     hunspell imagemagick inkscape latexmk lcov less libacl1-dev libbz2-dev \
     libc-dev libc6-dev-i386 libfontconfig1 libfreetype6-dev libgdbm-dev \
     libgif-dev libgit2-dev libgl1-mesa-dev libglfw3-dev libgmp-dev \
     libgnutls28-dev libgoogle-perftools-dev libgraphviz-dev libgsl-dev \
     libicu-dev libjansson-dev libjansson4 libjpeg-dev libjson-c-dev \
     liblapack-dev liblzma-dev libmagick++-dev libmpfr-dev libncurses5-dev \
     libopenmpi-dev libpng-dev libreadline6-dev libsdl2-dev libsdl2-gfx-dev \
     libsdl2-image-dev libsdl2-mixer-dev libsdl2-net-dev libsdl2-ttf-dev \
     libsqlite3-dev libssl-dev libsuitesparse-dev libtinfo5 libtool libtool-bin \
     libwebsockets-dev libxml2-dev libxrender1 libxslt1-dev libyaml-dev \
     libzmq3-dev libzstd-dev lmodern lzma lzma-dev make mingw-w64 \
     ninja-build nodejs npm pandoc parallel petsc-dev python2-dev \
     python3-dev python3-pip python3-setuptools python3-wheel ripgrep rr \
     rsync scons ssh sudo texinfo texlive-bibtex-extra texlive-fonts-extra \
     texlive-fonts-recommended texlive-lang-european texlive-latex-extra \
     texlive-latex-recommended texlive-science texlive-xetex time tk-dev \
     tmux tree trilinos-dev unzip valgrind wget wine-stable wkhtmltopdf \
     xorg-dev xutils-dev zip zlib1g-dev zstd

wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key|sudo apt-key add -
UBUNTU_CODENAME=$(grep UBUNTU_CODENAME /etc/os-release | cut -d= -f2)
echo "deb http://apt.llvm.org/$UBUNTU_CODENAME/ llvm-toolchain-$UBUNTU_CODENAME-11 main" | sudo tee -a /etc/apt/sources.list
echo "deb-src http://apt.llvm.org/$UBUNTU_CODENAME/ llvm-toolchain-$UBUNTU_CODENAME-11 main" | sudo tee -a /etc/apt/sources.list
sudo apt-get update
sudo apt-get --assume-yes --no-install-recommends install \
    clang-11 libllvm11 libclang-11-dev lldb-11 llvm-11 llvm-11-dev llvm-11-runtime clang-format-11 clang-tidy-11 libomp-11-dev libclang-11-dev
python3 -m pip install --upgrade-strategy=eager --upgrade pip setuptools wheel
python3 -m pip install libclang && python3 -c "from clang.cindex import Index; Index.create()"
