#!/bin/bash -ue
# usage:
#
#    $ ./test_repo.sh block_diag_ilu/
#
#    $ ./test_repo.sh pynleq2 -e PYNLEQ2_NLEQ2_ROOT_URL=http://secret.example.com/nleq2/
#
# Depends on drone CLI client:
#
#   http://readme.drone.io/devs/cli/
#

BASE_PATH=$(unset CDPATH && cd "$(dirname $0)" && echo $PWD)
REPO=${1%/}
REPO_PATH=${BASE_PATH}/${REPO}
if [[ ! -d ${REPO_PATH} ]]; then
    >&2 echo "${REPO_PATH} is not a directory"
    exit 1
fi
if [[ ! -f ${REPO_PATH}/.drone.yml ]]; then
    >&2 echo "No .drone.yml file in ${REPO_PATH}"
    exit 1
fi
if [[ ! -d "/tmp/_drone_ci_cache" ]]; then
    mkdir "/tmp/_drone_ci_cache"
fi
if [[ -d "/tmp/_drone_ci_cache/${REPO}_ci" ]]; then
    if [[ -d "/tmp/_drone_ci_cache/${REPO}_ci/.git" ]]; then
        rm -rf "/tmp/_drone_ci_cache/${REPO}_ci/.git"
    fi
fi
sudo rsync -rvauP --delete\
     --exclude "/build"\
     --exclude "/.cache"\
     --exclude "*.pyc"\
     --exclude "*.so"\
     --exclude "/benchmarks/"\
     --exclude "/dist/"\
     --exclude "/deploy/"\
     --exclude "/output/"\
     ${REPO_PATH}/ /tmp/_drone_ci_cache/${REPO}_ci/ >/dev/null
cleanup() {
    sudo chown -R $(id -u):$(id -g) /tmp/_drone_ci_cache/${REPO}_ci
}
trap cleanup INT TERM EXIT
if groups | grep docker >/dev/null; then
    DRONECMD=drone
else
    DRONECMD="sudo drone"
fi

( cd /tmp/_drone_ci_cache/${REPO}_ci; git clean -xfd; $DRONECMD exec --trusted ${@:2} )

