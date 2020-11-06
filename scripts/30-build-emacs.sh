#!/bin/bash
cd /tmp && \
        git clone --depth 1 --branch emacs-27 git://github.com/emacs-mirror/emacs && cd emacs && ./autogen.sh && \
        CC=gcc-10 CXX=g++-10 ./configure --with-imagemagick --with-modules  --prefix=/usr/local && \
        make && sudo make install && cd /tmp && rm
