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

LLVM_ROOT="/opt-2/llvm-19"
export EMACS_COMMANDS="\
(lsp-workspace-folders-add \"$LLVM_ROOT\") \
(split-window-below) \
(other-window 1) \
(find-file \"symengine/basic.h\")\
"

export CXXFLAGS="-ffunction-sections -fdata-sections ${CXXFLAGS:-''}"
export LDFLAGS="${LDFLAGS:-''}"

if [[ $USE_LLVM == 1 ]]; then
    export CC=gcc CXX=g++
    CMAKE_ARGS="-DWITH_LLVM:BOOL=ON $CMAKE_ARGS"
    export CXXFLAGS="-D_GLIBCXX_DEBUG -D_GLIBCXX_DEBUG_PEDANTIC -std=c++17 -O0 -ggdb3 $CXXFLAGS"
else
    export CC=clang CXX=clang++
    LIBCXX_ASAN_ROOT="/opt-2/libcxx19-asan"
    CMAKE_ARGS="-DWITH_LLVM:BOOL=OFF $CMAKE_ARGS"
    export CXXFLAGS="-D_LIBCPP_DEBUG -std=c++17 -fsanitize=address,undefined -nostdinc++ -I$LIBCXX_ASAN_ROOT/include -I$LIBCXX_ASAN_ROOT/include/c++/v1 -fno-omit-frame-pointer -fno-optimize-sibling-calls -O0 -glldb $CXXFLAGS"
    export LDFLAGS="-fsanitize=address,undefined -nostdlib++ -Wl,-rpath,/opt-2/libcxx19-asan/lib -L/opt-2/libcxx19-asan/lib -lc++ -lc++abi $LDFLAGS"
fi
EXTRA_ENV="-e CXXFLAGS -e LDFLAGS -e CC -e CXX"

if [[ $USE_BOOST == 1 ]]; then
    CMAKE_ARGS="$CMAKE_ARGS\
 -DINTEGER_CLASS=boostmp"
    export THIS_BUILD=${PWD}/build-boost-gccemacs-doom
else
    CMAKE_ARGS="$CMAKE_ARGS\
 -DWITH_GMP=ON \
 -DWITH_MPFR=ON \
 -DWITH_MPC=ON \
 -DINTEGER_CLASS=flint"
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
host-devenv-gccemacs-doom-cmake.sh "${ARGS[@]}"
