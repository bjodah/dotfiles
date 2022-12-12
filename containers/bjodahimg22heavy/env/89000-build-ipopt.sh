#!/bin/bash
set -euxo pipefail

IPOPT_VERSION=${1:-3.14.10}
PREFIX_ROOT=${2:-/opt/ipopt-${IPOPT_VERSION}}
BUILD_ROOT=${3:-/build}

SRC_DIR=${BUILD_ROOT}/ipopt-${IPOPT_VERSION}
if [[ ! -e $SRC_DIR ]]; then
    curl -Ls https://github.com/coin-or/Ipopt/archive/releases/${IPOPT_VERSION}.tar.gz | tar xz -C $BUILD_ROOT
    mv $BUILD_ROOT/Ipopt-releases-${IPOPT_VERSION} $SRC_DIR
fi

cd $SRC_DIR

if [ ! -e /usr/lib/x86_64-linux-gnu/libmetis.so ]; then
    if [ ! -e /usr/lib/x86_64-linux-gnu/libmetis.so.5 ]; then
        >&2 echo "Hardcoded work-around path for libmetis needs updating"
        exit 1
    fi
    ln -s /usr/lib/x86_64-linux-gnu/libmetis.so.5 /usr/lib/x86_64-linux-gnu/libmetis.so
fi

if [ ! -e /usr/lib/x86_64-linux-gnu/libgfortran.so ]; then
    if [ ! -e /usr/lib/x86_64-linux-gnu/libgfortran.so.5 ]; then
        >&2 echo "Hardcoded work-around path for libgfortran needs updating"
        exit 1
    fi
    ln -s /usr/lib/x86_64-linux-gnu/libgfortran.so.5 /usr/lib/x86_64-linux-gnu/libgfortran.so
fi

#export LDFLAGS="${LDFLAGS:-} -L${MUMPS_PREFIX}/lib -Wl,-rpath,${MUMPS_PREFIX}/lib"
if [ "$(uname)" == "Linux" ]; then
  export LDFLAGS="${LDFLAGS:-} -lrt"
fi

for CONFIG in asan glibcxxdbg dbg rel; do
    IPOPT_PREFIX=$PREFIX_ROOT-$CONFIG
    mkdir -p build-$CONFIG
    cd build-$CONFIG
    export FC=${GNU_FC:-gfortran}
    if [[ "$CONFIG" == rel ]]; then  # "release"
        CFLAGS_="-O2 -DNDEBUG ${CFLAGS:-}"
        CXXFLAGS_="-O2 -DNDEBUG ${CXXFLAGS:-}"
    else        
        if [[ "$CONFIG" == asan ]]; then
            CC=${CLANG_CC:-clang}
            CXX=${CLANG_CXX:-clang++}
            CFLAGS_="-fsanitize=address -Og -g -glldb ${CFLAGS:-}"
            CXXFLAGS_="-fsanitize=address -Og -g -glldb ${CXXFLAGS:-}"
            LDFLAGS_="-fsanitize=address ${LDFLAGS:-}"
        else
            CC=${GNU_CC:-gcc}
            CXX=${GNU_CXX:-g++}
            if [[ "$CONFIG" == glibcxxdbg ]]; then
                CFLAGS_="-D_GLIBCXX_DEBUG -D_GLIBCXX_DEBUG_PEDANTIC -O0 -g -ggdb3 ${CFLAGS:-}"
                CXXFLAGS_="-D_GLIBCXX_DEBUG -D_GLIBCXX_DEBUG_PEDANTIC -O0 -g -ggdb3 ${CXXFLAGS:-}"
            elif [[ "$CONFIG" == dbg ]]; then
                CFLAGS_="-Og -g -ggdb3 ${CFLAGS:-}"
                CXXFLAGS_="-Og -g -ggdb3 ${CXXFLAGS:-}"
            else
                >&2 echo "Unknown CONFIG: $CONFIG"
                exit 1
            fi
        fi
    fi

    MUMPS_PREFIX=/usr
    ASAN_OPTIONS=detect_leaks=0 ../configure \
        CXX="$CXX" CC="$CC" F77="$FC" \
        CFLAGS="${CFLAGS_}" CXXFLAGS="${CXXFLAGS_}" LDFLAGS="${LDFLAGS_}" \
        --without-hsl \
        --disable-java \
        --with-mumps \
        --with-mumps-cflags="-I${MUMPS_PREFIX}/include/mumps_seq" \
        --with-mumps-lflags="-ldmumps_seq -lmumps_common_seq -lpord_seq -lmpiseq_seq -lesmumps -lscotch -lscotcherr -lmetis -lgfortran" \
        --prefix=${IPOPT_PREFIX} || ( find . -name "config.log" | xargs cat )

    # --with-asl \
        # --with-asl-cflags="-I${PREFIX}/include/asl" \
        # --with-asl-lflags="-lasl" \

    bear -- make -j$(nproc) V=1
    ASAN_OPTIONS=detect_leaks=0 make test
    make install

    # for backward compatibility
    install -d ${IPOPT_PREFIX}/include/coin
    install -m644 ${IPOPT_PREFIX}/include/coin-or/* ${IPOPT_PREFIX}/include/coin

    find . -iname "*.o" | xargs rm
    cp compile_commands.json $IPOPT_PREFIX/
    cd - 
done
