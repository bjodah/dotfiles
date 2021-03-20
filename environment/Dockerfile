FROM bjodah/bjodahimg20:21.3
MAINTAINER Björn Dahlgren <bjodah@DELETEMEgmail.com>

COPY ???-*.sh *.el /opt/

RUN \
    /opt/100-clone-dotfiles.sh && \
    /opt/110-setup-emacs.sh && \
    chmod 666 -R /opt/bjodah-dotfiles && \
    find /opt/bjodah-dotfiles -type d | xargs chmod 777 && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* ~/.cache/pip
