# -*- mode: dockerfile -*-
# -*- compile-command: "podman build ." -*-

FROM bjodahimg22heavy:22.6.a.1.1.1


# RUN apt-get update \
#     && apt-get --quiet --assume-yes upgrade \
#     && apt-get --quiet --assume-yes install apt-transport-https ca-certificates \
#     && update-ca-certificates \
#     && apt-get --quiet --assume-yes --no-install-recommends install \
#         ccache \
#         cmake \
#         curl \
#         fish \
#         git \
#         gnupg2 \
#         libgmp-dev \
#         libjson-c-dev \
#         libtool-bin \
#         libvterm-dev \
#         libwebsockets-dev \
#         make \
#         ninja-build \
#         python3-pip\
#         ripgrep \
#         tmux \
#         unzip \
#     && apt-get clean \
#     && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# COPY 11-setup_llvm_clang.sh /opt/

# RUN /opt/11-setup_llvm_clang.sh 14 \
#     && apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

ENV CXX=clang++-14 CC=clang-14 PATH=/opt/emacs-29pg/bin:$PATH

RUN git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d \
    && yes | ~/.emacs.d/bin/doom install

RUN git clone --depth 1 https://github.com/akermu/emacs-libvterm /opt/emacs-libvterm && \
    cd /opt/emacs-libvterm/; mkdir build; cmake -DCMAKE_INSTALL_PREFIX=/opt/emacs-29pg/share/emacs/site-lisp -S . -B build && \
    cmake --build ./build/ && \
    cmake --install ./build && cp vterm-module.so /opt/emacs-29pg/share/emacs/site-lisp


ADD doom-base-config.tar.gz /root/

COPY .config/fish/config.fish /root/.config/fish/

RUN ~/.emacs.d/bin/doom sync

COPY ./.doom.d/config.el ./.doom.d/packages.el ./.doom.d/init.el /root/.doom.d/

RUN ~/.emacs.d/bin/doom sync

RUN apt-get update \
    && apt-get --assume-yes --no-install-recommends install \
        libmpc-dev
