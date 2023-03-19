#!/bin/bash
set -euo pipefail

show_help() {
    echo "Host a development environment (emacs) for symengine using a container."
    echo ""
    echo "Usage:"
    echo "--llvm      Enable LLVM backend"
    echo "--boost     Use boostmp instead of flint"
    echo "Example:"
    echo ""
    echo '  $ cd ~/vc/symengine && $BASH_SOURCE --boost -- --host-ttyd 7762 --emacs-flags "-nw" -- --x11'
    echo '  $ host-SymEngine-devenv-gccemacs-doom-cmake.sh -- -- -e CXXFLAGS="-Wno-inconsistent-missing-override"'
}

if ! grep project CMakeLists.txt | grep symengine; then
    >&2 echo "Not in symengine root dir?"
    exit 1
fi


USE_BOOST=0
USE_LLVM=0
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
        --llvm)
            USE_LLVM=1
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
set -x

CMAKE_ARGS="\
 -DCMAKE_BUILD_TYPE=Debug \
 -DBUILD_SHARED_LIBS=ON \
 -DBUILD_TESTS=ON \
 -DBUILD_BENCHMARKS=ON \
 -DHAVE_GCC_ABI_DEMANGLE=no"


export EMACS_COMMANDS="\
(lsp-workspace-folders-add \"/usr/include/llvm-16/\") \
(split-window-below) \
(other-window 1) \
(find-file \"symengine/basic.h\")\
"

if [[ $USE_LLVM == 1 ]]; then
    CMAKE_ARGS="-DWITH_LLVM:BOOL=ON $CMAKE_ARGS"
    EXTRA_ENV=""
else
    CMAKE_ARGS="-DWITH_LLVM:BOOL=OFF $CMAKE_ARGS"
    export CXXFLAGS="-std=c++17 -fsanitize=address -stdlib=libc++ -nostdinc++ -I/opt-2/libcxx16-debug/include -I/opt-2/libcxx16-debug/include/c++/v1 -fno-omit-frame-pointer -fno-optimize-sibling-calls -O0 -glldb"
    export LDFLAGS="-fsanitize=address -Wl,-rpath,/opt-2/libcxx16-debug/lib -L/opt-2/libcxx16-debug/lib -lc++abi"
    EXTRA_ENV="-e CXXFLAGS -e LDFLAGS"
fi

if [[ $USE_BOOST == 1 ]]; then
    CMAKE_ARGS="$CMAKE_ARGS\
 -DINTEGER_CLASS=boostmp"
    export THIS_BUILD=${PWD}/build-boost-gccemacs-doom
    #EXTRA_ENV="-e CMAKE_PREFIX_PATH=/opt2/boost-1.81.0 $EXTRA_ENV"
else
    CMAKE_ARGS="$CMAKE_ARGS\
 -DWITH_GMP=ON \
 -DWITH_MPFR=ON \
 -DWITH_MPC=ON \
 -DINTEGER_CLASS=flint"
    EXTRA_ENV="-e FLINT_ROOT=/opt-2/flint2-2.9.0-release $EXTRA_ENV"
    export THIS_BUILD=${PWD}/build-flint-gccemacs-doom
fi


INSERTED=0
declare -a ARGS
while [ $# -gt 0 ]; do
    case "$1" in
        --)
            ARGS+=("$1")
            ARGS+=("$EXTRA_ENV")
            ARGS+=("-e CMAKE_GENERATOR=Ninja")
            INSERTED=1
            shift
            ;;
        *)
            ARGS+=("$1")
            shift
            ;;
    esac
done
if [[ $INSERTED == 0 ]]; then
    ARGS+=("--")
    ARGS+=("$EXTRA_ENV")
    ARGS+=("-e CMAKE_GENERATOR=Ninja")
fi

export CMAKE_ARGS
host-devenv-gccemacs-doom-cmake.sh ${ARGS[@]}
