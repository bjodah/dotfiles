#!/bin/bash -xe
sudo apt-get --assume-yes --no-install-recommends install \
     lldb-15 inkscape texlive-bibtex-extra pandoc cm-super texlive-fonts-extra gfortran-12 libicu-dev wine-stable plocate

sudo update-alternatives --install /usr/bin/gfortran gfortran /usr/bin/gfortran-12 1000
sudo update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-12 1000
sudo update-alternatives --set cc /usr/bin/gcc

sudo update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-12 1000
#sudo update-alternatives --set c++ /usr/bin/g++

# trilinos-dev
