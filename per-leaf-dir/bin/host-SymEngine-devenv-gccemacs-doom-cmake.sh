#!/bin/bash
set -euxo pipefail

show_help() {
    echo "Host a development environment (emacs) for symengine using a container."
    echo ""
    echo "Usage:"
    echo "--boost     Use boostmp instead of flint"
    echo "Example:"
    echo ""
    echo '  $ cd ~/vc/symengine && $BASH_SOURCE --boost -- --host-ttyd 7762 --emacs-flags "-nw"'
}

USE_BOOST=0
while [ $# -gt 0 ]; do
    case "$1" in
        -h|--help|\?)
            show_help
            exit 0
            ;;
        --boost)
            USE_BOOST=1
            shift
            ;;
        --)
            shift
            break;
            ;;
        *)
            >&2 echo "Unrecognized flag: $1, pass --help fpr more info, aborting..."
            exit 1
    esac
done

CMAKE_ARGS="\
 -DCMAKE_BUILD_TYPE=Debug \
 -DBUILD_SHARED_LIBS=ON \
 -DBUILD_TESTS=ON \
 -DBUILD_BENCHMARKS=ON \
 -DWITH_LLVM:BOOL=ON"


export EMACS_COMMANDS="\
(lsp-workspace-folders-add \"/usr/include/llvm-15/\") \
(split-window-below) \
(other-window 1) \
(find-file \"symengine/basic.h\")\
"

if [[ $USE_BOOST == 1 ]]; then
    CMAKE_ARGS="\
 -DINTEGER_CLASS=boostmp"
    EXTRA_ENV=""
    export THIS_BUILD=build-boost-gccemacs-doom
else
    CMAKE_ARGS="\
 -DWITH_GMP=ON \
 -DWITH_MPFR=ON \
 -DWITH_MPC=ON \
 -DINTEGER_CLASS=flint"
    EXTRA_ENV="-e FLINT_ROOT=/opt/flint2-2.9.0-debug"
    export THIS_BUILD=build-flint-gccemacs-doom
fi


export CMAKE_ARGS
host-devenv-gccemacs-doom-cmake.sh \
    $@ -- \
    $EXTRA_ENV \
    -e CMAKE_GENERATOR=Ninja
