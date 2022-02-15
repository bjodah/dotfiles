#!/bin/bash
set -euxo pipefail
TARGET=$1
mkdir -p $TARGET
curl -Ls https://github.com/bjodah/dotfiles/archive/master.tar.gz | tar xz -C $TARGET --strip-components=1
