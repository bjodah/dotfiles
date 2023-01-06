#!/bin/bash -xe
sudo apt-get --assume-yes --no-install-recommends install \
     lldb-15 inkscape texlive-bibtex-extra pandoc cm-super texlive-fonts-extra gfortran-12 libicu-dev wine-stable plocate

sudo update-alternatives --install /usr/bin/gfortran gfortran /usr/bin/gfortran-12 1000

# trilinos-dev
