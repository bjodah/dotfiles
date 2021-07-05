FROM ubuntu:focal-20210416
MAINTAINER Björn Dahlgren <bjodah@DELETEMEgmail.com>
ARG DEBIAN_FRONTEND=noninteractive
ARG DEBCONF_NONINTERACTIVE_SEEN=true
ENV LANG=en_US.UTF-8 LANGUAGE=en_US:en LC_ALL=en_US.UTF-8 TZ=Europe/Stockholm

# localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8 && \

COPY 1?-*.sh 2?-*.sh /opt/

RUN apt-get update && \
    apt-get --quiet --assume-yes --no-install-recommends install locales man manpages sudo tzdata && \
    echo "LC_ALL=en_US.UTF-8" >> /etc/environment && \
    echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen && \
    echo "LANG=en_US.UTF-8" > /etc/locale.conf && \
    locale-gen en_US.UTF-8 && \
    apt-get dist-upgrade -y && \
    /opt/10-setup_ubuntu_packages.sh && \
    /opt/11-setup_llvm_clang.sh && \
    /opt/12-setup-gcc.sh && \
    /opt/13-setup_intel_compilers.sh && \
    /opt/15-pip-install.sh && \
    /opt/16-ccls.sh && \
    /opt/17-build-emacs.sh && \
    /opt/19-java.sh && \
    /opt/21-npm-language-servers.sh && \
    /opt/25-openblas.sh -DTARGET=NEHALEM -DARCH=x86_64 && \
    /opt/27-sundials.sh && \
    apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* ~/.cache/pip

COPY 3?-*.sh 4?-*.sh 6?-*.sh *.patch /opt/

RUN /opt/33-libcxx-msan.sh && \
    /opt/40-cpython-dbg.sh && \
    mkdir /opt/cargo /opt/rustup && CARGO_HOME=/opt/cargo PATH="$CARGO_HOME/bin:$PATH" RUSTUP_HOME=/opt/rustup /opt/67-rust.sh && \
    apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* ~/.cache/pip

COPY 7?-*.sh /opt/
RUN /opt/75-boost.sh
