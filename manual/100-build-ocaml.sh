#!/bin/bash
set -euxo pipefail
PREFIX=${1:-"/opt/ocaml-4.13.1"}
mkdir -p /build
curl -Ls https://github.com/ocaml/ocaml/archive/refs/tags/4.13.1.tar.gz | tar xz -C /build
cd /build/ocaml-4.13.1
./configure --prefix="$PREFIX"
make -j $(nproc)
make install
