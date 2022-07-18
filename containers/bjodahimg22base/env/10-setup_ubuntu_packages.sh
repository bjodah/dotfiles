#!/bin/bash -xe
sudo apt-get --assume-yes --no-install-recommends install \
     bat bc bear biber bison ca-certificates ccache cmake cppcheck \
     curl dirmngr dot2tex dvipng expect fd-find ffmpeg flex fonts-dejavu \
     fonts-humor-sans fish fzf git gnupg2 gnuplot graphviz \
     htop hunspell imagemagick latexmk lcov less lmodern lzma make \
     ninja-build parallel pngquant poppler-utils pv \
     python3-pip python3-setuptools python3-wheel ripgrep \
     rsync scons ssh texinfo \
     texlive-fonts-recommended texlive-lang-european texlive-latex-extra \
     texlive-latex-recommended texlive-science texlive-xetex time \
     tmux tree unison unzip wget \
     xz-utils zip zstd

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
