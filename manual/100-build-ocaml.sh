#!/bin/bash
set -euxo pipefail
PREFIX=${1:-"/opt/ocaml-4.13.1"}
BUILD_ROOT=${2:-"/build"}
OCAML_VERSION=${3:-"4.13.1"}
mkdir -p $BUILD_ROOT
curl -Ls https://github.com/ocaml/ocaml/archive/refs/tags/${OCAML_VERSION}.tar.gz | tar xz -C $BUILD_ROOT
cd ${BUILD_ROOT}/ocaml-${OCAML_VERSION}
./configure --prefix="$PREFIX"
make -j $(nproc)
make install
