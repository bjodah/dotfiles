FROM bjodah/bjodahimg20:21.5
MAINTAINER Björn Dahlgren <bjodah@DELETEMEgmail.com>

COPY ???-*.sh *.el /opt/

RUN \
    /opt/100-clone-dotfiles.sh /opt/bjodah-dotfiles && \
    /opt/110-setup-emacs.sh    /opt/bjodah-dotfiles && \
    chmod 666 -R               /opt/bjodah-dotfiles && \
    find                       /opt/bjodah-dotfiles -type d | xargs chmod 777 && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* ~/.cache/pip

# RUN \
#         cd /tmp && \
#         git clone --branch clang_12 git://github.com/include-what-you-use/include-what-you-use && \
#         CXX=clang++-12 cmake -S /tmp/include-what-you-use -B /tmp/include-what-you-use/build && \
#         cmake --build /tmp/include-what-you-use/build && \
#         cmake --install /tmp/include-what-you-use/build && \
#         rm -r /tmp/include-what-you-use/ && \
#         include-what-you-use --help
