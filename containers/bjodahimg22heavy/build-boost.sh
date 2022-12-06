#!/bin/bash
set -euxo pipefail
cd $(dirname $BASH_SOURCE)

podman run \
       --rm \
       -v $HOME/.ccahe:/root/.ccahe \
       -v $(pwd):/work \
       -w /work \
       -it $(grep FROM env/Containerfile | head -n 1 | cut -d' ' -f2) \
       bash -c "set -euxo pipefail; ./750-boost.sh"

cp -a opt_boost*.tar.zst ./env/
