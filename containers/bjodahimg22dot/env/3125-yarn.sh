#!/bin/bash
set -euxo pipefail
YARN_VERSION=1.22.18
PREFIX="${1:-/opt/yarn-1.22}"
if [ ! -d "$PREFIX" ]; then
    mkdir -p "$PREFIX"
fi
savedAptMark="$(apt-mark showmanual)"
for key in \
    6A010C5166006599AA17F08146C2130DFD2497F5 \
  ; do \
    gpg2 --batch --keyserver hkps://keys.openpgp.org --recv-keys "$key" || \
    gpg2 --batch --keyserver keyserver.ubuntu.com --recv-keys "$key" ; \
  done
curl -fsSLO --compressed "https://yarnpkg.com/downloads/$YARN_VERSION/yarn-v$YARN_VERSION.tar.gz"
curl -fsSLO --compressed "https://yarnpkg.com/downloads/$YARN_VERSION/yarn-v$YARN_VERSION.tar.gz.asc"
gpg2 --batch --verify yarn-v$YARN_VERSION.tar.gz.asc yarn-v$YARN_VERSION.tar.gz

tar -xzf yarn-v$YARN_VERSION.tar.gz -C $PREFIX --strip-components=1 --no-same-owner
rm yarn-v$YARN_VERSION.tar.gz.asc yarn-v$YARN_VERSION.tar.gz
PATH=$PREFIX/bin:$PATH yarn --version
