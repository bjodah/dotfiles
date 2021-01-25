FROM ubuntu:focal-20210119
MAINTAINER Björn Dahlgren <bjodah@DELETEMEgmail.com>
ARG DEBIAN_FRONTEND=noninteractive
ARG DEBCONF_NONINTERACTIVE_SEEN=true
ENV LANG=en_US.UTF-8 LANGUAGE=en_US:en LC_ALL=en_US.UTF-8 TZ=Europe/Stockholm

# localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8 && \

COPY ??-*.sh *.patch /opt/

RUN apt-get update && \
    apt-get --quiet --assume-yes --no-install-recommends install locales man manpages sudo tzdata && \
    echo "LC_ALL=en_US.UTF-8" >> /etc/environment && \
    echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen && \
    echo "LANG=en_US.UTF-8" > /etc/locale.conf && \
    locale-gen en_US.UTF-8 && \
    apt-get dist-upgrade -y && \
    /opt/10-setup_ubuntu_packages.sh && \
    mkdir /build && /opt/13-build-emacs.sh && \
    /opt/14-ccls.sh && \
    /opt/15-pip-install.sh && \
    /opt/19-java.sh && \
    /opt/21-npm-language-servers.sh && \
    /opt/25-openblas.sh && \
    /opt/27-sundials.sh && \
    /opt/33-libcxx-msan.sh && \
    /opt/40-cpython-dbg.sh && \
    mkdir /opt/cargo /opt/rustup; CARGO_HOME=/opt/cargo PATH="$CARGO_HOME/bin:$PATH" RUSTUP_HOME=/opt/rustup /opt/67-rust.sh && \
    apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* ~/.cache/pip

COPY *.diff /opt/
RUN        ls -l /opt/*.diff; sed -i 's/\.patch/.diff/g; s/#curl/curl/g' /opt/34-boost.sh; /opt/34-boost.sh
#RUN        /opt/70-julia.sh
