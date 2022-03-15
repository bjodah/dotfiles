#!/bin/bash
set -euxo pipefail
PREFIX=${1:-"/usr/local"}
mkdir -p /build
curl -Ls https://github.com/bcpierce00/unison/archive/refs/tags/v2.51.5.tar.gz | tar xz -C /build
cd /build/unison-2.51.5
PATH=/opt/ocaml-4.13.1/bin:$PATH make text
cp ./src/unison $PREFIX/bin/
