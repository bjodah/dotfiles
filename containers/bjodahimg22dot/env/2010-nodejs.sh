#!/bin/bash
# adapted from:
# https://github.com/nodejs/docker-node/blob/main/16/buster-slim/Dockerfile

NODE_VERSION=16.19.0  # 18.12.1
set -euxo pipefail
PREFIX="${1:-/opt/node-16}"
if [ ! -d "$PREFIX" ]; then
    mkdir -p "$PREFIX"
fi

ARCH=
dpkgArch="$(dpkg --print-architecture)" 
case "${dpkgArch##*-}" in \
      amd64) ARCH='x64';; \
      ppc64el) ARCH='ppc64le';; \
      s390x) ARCH='s390x';; \
      arm64) ARCH='arm64';; \
      armhf) ARCH='armv7l';; \
      i386) ARCH='x86';; \
      *) echo "unsupported architecture"; exit 1 ;;
esac 
for key in \
      4ED778F539E3634C779C87C6D7062848A1AB005C \
      94AE36675C464D64BAFA68DD7434390BDBE9B9C5 \
      74F12602B6F1C4E913FAA37AD3A89613643B6201 \
      71DCFD284A79C3B38668286BC97EC7A07EDE3FC1 \
      8FCCA13FEF1D0C2E91008E09770F7A9A5AE15600 \
      C4F0DFFF4E8C1A8236409D08E73BC641CC11F4C8 \
      C82FA3AE1CBEDC6BE46B9360C43CEC45C17AB93C \
      DD8F2338BAE7501E3DD5AC78C273792F7D83545D \
      A48C2BEE680E841632CD4E44F07496B3EB3C1762 \
      108F52B48DB57BB0CC439B2997B01419BD92F80A \
      B9E2F5981AA6E0CD28160D9FF13993A75599653C \
    ; do
      gpg2 --batch --keyserver hkps://keys.openpgp.org --recv-keys "$key" || \
      gpg2 --batch --keyserver keyserver.ubuntu.com --recv-keys "$key" ; \
    done 
curl -fsSLO --compressed "https://nodejs.org/dist/v$NODE_VERSION/node-v$NODE_VERSION-linux-$ARCH.tar.xz" 
curl -fsSLO --compressed "https://nodejs.org/dist/v$NODE_VERSION/SHASUMS256.txt.asc" 
if [ -e SHASUMS256.txt ]; then
    rm SHASUMS256.txt
fi
gpg2 --batch --decrypt --output SHASUMS256.txt SHASUMS256.txt.asc 
grep " node-v$NODE_VERSION-linux-$ARCH.tar.xz\$" SHASUMS256.txt | sha256sum -c - 
tar -xJf "node-v$NODE_VERSION-linux-$ARCH.tar.xz" -C $PREFIX --strip-components=1 --no-same-owner 
rm "node-v$NODE_VERSION-linux-$ARCH.tar.xz" SHASUMS256.txt.asc SHASUMS256.txt 
ln -s $PREFIX/bin/node $PREFIX/bin/nodejs 
PATH=$PREFIX/bin:$PATH node --version 
PATH=$PREFIX/bin:$PATH npm --version
