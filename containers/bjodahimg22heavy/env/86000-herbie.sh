#!/bin/sh
echo -n 'deb http://ppa.launchpad.net/plt/racket/ubuntu jammy main' > /etc/apt/sources.list.d/plt-ubuntu-racket-jammy.list
apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv D9D33FCD84D82C17288BA03B3C9A6980F827E01E
apt-get update --quiet
apt-get  --assume-yes --no-install-recommends install racket
apt-get clean
git clone --depth 1 https://github.com/uwplse/herbie /opt/herbie
cd /opt/herbie
PATH=/opt/cargo/bin:$PATH make install
