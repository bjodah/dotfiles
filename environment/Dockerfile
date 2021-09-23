FROM bjodah/bjodahimg20:21.8
MAINTAINER Björn Dahlgren <bjodah@DELETEMEgmail.com>

COPY ???-*.sh *.el /opt/

RUN \
    /opt/100-clone-dotfiles.sh /opt/bjodah-dotfiles && \
    /opt/110-setup-emacs.sh    /opt/bjodah-dotfiles && \
    chmod 666 -R               /opt/bjodah-dotfiles && \
    find                       /opt/bjodah-dotfiles -type d | xargs chmod 777 && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* ~/.cache/pip

RUN \
    git clone --branch clang_12 https://github.com/include-what-you-use/include-what-you-use.git /build/iwyu-12-src && \
    CXX=clang++-12 CC=clang-12 cmake -S /build/iwyu-12-src -B /build/iwyu-12-bld -DCMAKE_BUILD_TYPE=Release -DCMAKE_PREFIX_PATH=/usr/lib/llvm-12 && \
    cmake --build /build/iwyu-12-bld && cmake --install /build/iwyu-12-bld && \
    curl -Ls https://gitlab.com/libeigen/eigen/-/archive/3.4.0/eigen-3.4.0.tar.bz2 | tar xj -C /build && \
    CXX=clang++-12 CC=clang-12 cmake -S /build/eigen-3.4.0/ -B /build/eigen-3.4.0-bld -DCMAKE_INSTALL_PREFIX=/opt/eigen-3.4.0 -DCMAKE_EXPORT_COMPILE_COMMANDS=ON && \
    cmake --build /build/eigen-3.4.0-bld && cmake --install /build/eigen-3.4.0-bld && \
    python3 -m pip install conan && \
        CPATH=/opt/eigen-3.4.0/include/eigen3 CC=gcc-11 CXX=g++-11 python3 -m pip install accupy dufte perfplots && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* ~/.cache/pip


RUN apt-add-repository ppa:fish-shell/release-3 && apt-get update && apt-get --assume-yes install fish pngquant pv && \
        update-alternatives --install  /usr/bin/python python /usr/bin/python3 1000 && \
        update-alternatives --install  /usr/bin/gcc gcc /usr/bin/gcc-11 1000 && \
        update-alternatives --install  /usr/bin/g++ g++ /usr/bin/g++-11 1000 && \
        update-alternatives --install  /usr/bin/gfortran gfortran /usr/bin/gfortran-11 1000 && \
        update-alternatives --install  /usr/bin/clang clang /usr/bin/clang-12 1000 && \
        update-alternatives --install  /usr/bin/clang++ clang++ /usr/bin/clang++-12 1000 && \
        update-alternatives --install  /usr/bin/clang-format clang-format /usr/bin/clang-format-12 1000 && \
        update-alternatives --install  /usr/bin/clang-tidy clang-tidy /usr/bin/clang-tidy-12 1000 && \
        apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN \
        curl -Ls https://www.davidhbailey.com/dhbsoftware/qd-2.3.22.tar.gz | tar xz -C /build && \
        cd /build/qd-2.3.22/ && CXX=g++-11 CC=gcc-11 FC=gfortran-11 ./configure --enable-shared --prefix=/opt/qd-2.3.22 && \
        bear make && make install && ln -s /build/qd-2.3.22/compile_commands.json /opt/qd-2.3.22/


COPY 15??-*.sh /opt/
RUN /opt/1500-clang-13.sh && \
    /opt/1533-libcxx-msan-beta.sh

#RUN /root/1000a-fix-emacs.sh

# RUN \
#         cd /tmp && \
#         git clone --branch clang_12 git://github.com/include-what-you-use/include-what-you-use && \
#         CXX=clang++-12 cmake -S /tmp/include-what-you-use -B /tmp/include-what-you-use/build && \
#         cmake --build /tmp/include-what-you-use/build && \
#         cmake --install /tmp/include-what-you-use/build && \
#         rm -r /tmp/include-what-you-use/ && \
#         include-what-you-use --help

COPY 2020-nodejs-lts.sh /opt/
RUN /opt/2020-nodejs-lts.sh

# Mistakes were made, we need sundials in a few more configs
COPY 27-sundials-fix.sh /opt/
RUN /opt/27-sundials-fix.sh

RUN \
        python3 -m pip uninstall -y python-language-server && \
        /opt/cpython-3.9-release/bin/python3.9 -m pip uninstall -y python-language-server && \
        /opt/cpython-3.9-debug/bin/python3.9 -m pip uninstall -y python-language-server && \
        python3 -m pip install poetry shed symengine python-lsp-server && \
        /opt/cpython-3.9-release/bin/python3.9 -m pip install poetry shed symengine python-lsp-server && \
        /opt/cpython-3.9-debug/bin/python3.9 -m pip install poetry shed symengine python-lsp-server && \
        CPATH=/opt/eigen-3.4.0/include/eigen3 CC=gcc-11 CXX=g++-11 /opt/cpython-3.9-release/bin/python3.9 -m pip install accupy dufte perfplots && \
        CPATH=/opt/eigen-3.4.0/include/eigen3 CC=gcc-11 CXX=g++-11 /opt/cpython-3.9-debug/bin/python3.9 -m pip install accupy dufte perfplots && \
        git clone https://github.com/magicmonty/bash-git-prompt.git /opt/bash-git-prompt --depth 1 && \
        rm -rf /tmp/* /var/tmp/* ~/.cache/pip


#yes | unminimize && \


# COPY init.el_ /root/emacs.d/init.el
# RUN emacs --batch --eval '(progn \
# (require (quote package))\
# (package-initialize)\
# (package-refresh-contents)\
# (package-install (quote use-package))\
#         (load-file "~/.emacs.d/init.el"))'
