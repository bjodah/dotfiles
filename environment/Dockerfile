FROM bjodah/bjodahimg22:21.12
MAINTAINER Björn Dahlgren <bjodah@DELETEMEgmail.com>

COPY ???-*.sh *.el /opt/

RUN \
    /opt/100-clone-dotfiles.sh /opt/bjodah-dotfiles && \
    /opt/110-setup-emacs.sh    /opt/bjodah-dotfiles && \
    chmod 666 -R               /opt/bjodah-dotfiles && \
    find                       /opt/bjodah-dotfiles -type d | xargs chmod 777 && \
    git clone https://github.com/magicmonty/bash-git-prompt.git /opt/bash-git-prompt --depth 1 && \
    update-alternatives --install  /usr/bin/python python /usr/bin/python3 1000 && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* ~/.cache/pip

RUN \
    git clone --branch clang_13 https://github.com/include-what-you-use/include-what-you-use.git /build/iwyu-13-src && \
    CXX=clang++-13 CC=clang-13 cmake -S /build/iwyu-13-src -B /build/iwyu-13-bld -DCMAKE_BUILD_TYPE=Release -DCMAKE_PREFIX_PATH=/usr/lib/llvm-13 && \
    cmake --build /build/iwyu-13-bld && cmake --install /build/iwyu-13-bld && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* ~/.cache/pip

    # curl -Ls https://gitlab.com/libeigen/eigen/-/archive/3.4.0/eigen-3.4.0.tar.bz2 | tar xj -C /build && \
    # CXX=clang++-13 CC=clang-13 cmake -S /build/eigen-3.4.0/ -B /build/eigen-3.4.0-bld -DCMAKE_INSTALL_PREFIX=/opt/eigen-3.4.0 -DCMAKE_EXPORT_COMPILE_COMMANDS=ON && \
    # cmake --build /build/eigen-3.4.0-bld && cmake --install /build/eigen-3.4.0-bld && \


# RUN \
#         update-alternatives --install  /usr/bin/gcc gcc /usr/bin/gcc-12 1000 && \
#         update-alternatives --install  /usr/bin/g++ g++ /usr/bin/g++-12 1000 && \
#         update-alternatives --install  /usr/bin/gfortran gfortran /usr/bin/gfortran-12 1000 && \
#         update-alternatives --install  /usr/bin/clang clang /usr/bin/clang-14 1000 && \
#         update-alternatives --install  /usr/bin/clang++ clang++ /usr/bin/clang++-14 1000 && \
#         update-alternatives --install  /usr/bin/clang-format clang-format /usr/bin/clang-format-14 1000 && \
#         update-alternatives --install  /usr/bin/clang-tidy clang-tidy /usr/bin/clang-tidy-14 1000 && \
#         apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN \
        curl -Ls https://www.davidhbailey.com/dhbsoftware/qd-2.3.22.tar.gz | tar xz -C /build && \
        cd /build/qd-2.3.22/ && CXX=g++-11 CC=gcc-11 FC=gfortran-11 ./configure --enable-shared --prefix=/opt/qd-2.3.22 && \
        bear make && make install && ln -s /build/qd-2.3.22/compile_commands.json /opt/qd-2.3.22/


COPY 15??-*.sh /opt/
RUN /opt/1500-clang-14.sh && \
    /opt/1533-libcxx-msan-beta.sh

#RUN /root/1000a-fix-emacs.sh

COPY 2020-nodejs-lts.sh /opt/
RUN /opt/2020-nodejs-lts.sh

# Mistakes were made, we need sundials in a few more configs
COPY 27-sundials-fix.sh /opt/
RUN /opt/27-sundials-fix.sh

RUN \
        python3 -m pip uninstall -y python-language-server && \
        /opt/cpython-3.10-release/bin/python3.10 -m pip uninstall -y python-language-server && \
        /opt/cpython-3.10-debug/bin/python3.10 -m pip uninstall -y python-language-server && \
        python3 -m pip install poetry shed symengine python-lsp-server && \
        /opt/cpython-3.10-release/bin/python3.10 -m pip install poetry shed symengine python-lsp-server && \
        /opt/cpython-3.10-debug/bin/python3.10 -m pip install poetry shed symengine python-lsp-server && \
        rm -rf /tmp/* /var/tmp/* ~/.cache/pip

RUN \
        mkdir /opt/intel-sde && \
        curl -Ls https://software.intel.com/content/dam/develop/external/us/en/documents/downloads/sde-external-8.69.1-2021-07-18-lin.tar.bz2 | tar xj -C /opt/intel-sde --strip-components=1

#yes | unminimize && \


# COPY init.el_ /root/emacs.d/init.el
# RUN emacs --batch --eval '(progn \
# (require (quote package))\
# (package-initialize)\
# (package-refresh-contents)\
# (package-install (quote use-package))\
#         (load-file "~/.emacs.d/init.el"))'
