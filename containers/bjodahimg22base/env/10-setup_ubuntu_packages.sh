#!/bin/bash -xe
sudo apt-get --assume-yes --no-install-recommends install \
     g++-12 gcc-12

sudo update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-12 1000
sudo update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-12 1000

sudo apt-get --assume-yes --no-install-recommends install \
     bat bc bear biber bison bzip2 ca-certificates ccache cdbs cmake cppcheck \
     curl devscripts dirmngr dot2tex dvipng expect fd-find ffmpeg flex fonts-dejavu \
     fonts-humor-sans fish fzf gawk git gnupg2 gnuplot graphviz \
     htop hunspell imagemagick latexmk lcov less libbz2-dev libc-dev libgmp-dev libmpfr-dev \
     libopenblas-openmp-dev lmodern lzma make \
     ninja-build parallel pngquant poppler-utils pv \
     python3-pip python3-setuptools python3-wheel python3-dev python3-gmpy2 \
     rsync scons ssh texinfo \
     texlive-fonts-recommended texlive-lang-european texlive-latex-extra \
     texlive-latex-recommended texlive-science texlive-xetex time \
     tmux tree unison unzip wget \
     xz-utils zip zlib1g-dev zstd

sudo update-alternatives --install /usr/bin/python python /usr/bin/python3 1000


# Heavy-weights among kept packages:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# cmake                      21.2 MB
# git                        18.8 MB
# lmodern                    34.1 MB
# texinfo                    11.6 MB
# texlive-fonts-recommended  15.4 MB
# texlive-lang-european      25.6 MB
# texlive-latex-extra        66.5 MB
# texlive-latex-recommended  31.8 MB
# texlive-science            18.6 MB
# texlive-xetex              18.4 MB

# Useful packages deferred for now (too heavy)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# inkscape                   86.2 MB
# texlive-bibtex-extra       116 MB
# pandoc                     155 MB
# cm-super                   53.9 MB

# texlive-fonts-extra 1383 MB
