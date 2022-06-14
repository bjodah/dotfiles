#!/bin/bash
set -euxo pipefail
cd $(dirname $BASH_SOURCE)

podman run --rm -v $(pwd):/work -w /work -it bjodah/bjodahimg22dev:22.6.a.1 bash -c "set -euxo pipefail; \
./1700-build-emacs.sh --git-branch emacs-28 --install /opt/emacs-28 --create-deb /work --build-root /build/emacs-28; \
./1700-build-emacs.sh --git-branch master --install /opt/emacs-29pg --create-deb /work --build-root /build/emacs-29 --with-native-comp --with-pgtk"
cp -a *.deb ./env/
