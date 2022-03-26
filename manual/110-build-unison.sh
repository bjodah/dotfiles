#!/bin/bash
set -uxo pipefail
if ! which ocaml; then
    >&2 echo "ocaml not on path?"
else
    ocaml --version
fi
set -e
PREFIX=${1:-"/usr/local"}
BUILD_ROOT=${2:-"/build"}
UNISON_VERSION=${3:-"2.51.5"}
mkdir -p ${BUILD_ROOT}
curl -Ls https://github.com/bcpierce00/unison/archive/refs/tags/v${UNISON_VERSION}.tar.gz | tar xz -C ${BUILD_ROOT}
cd ${BUILD_ROOT}/unison-${UNISON_VERSION}
make text
cp ./src/unison $PREFIX/bin/
