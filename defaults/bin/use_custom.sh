export CMAKE_GENERATOR=Ninja
export CONAN_CMAKE_GENERATOR=Ninja

add_prefix_to_compiler_env_vars(){
    export CPATH=$1/include:$CPATH
    export LIBRARY_PATH=$1/lib:$LIBRARY_PATH
    export LD_LIBRARY_PATH=$1/lib:$LD_LIBRARY_PATH
}

sundials_fix(){
    # see e.g. http://sundials.2283335.n4.nabble.com/sundials-3-and-4-td4654629.html
    export CPATH=/usr/include/suitesparse:$CPATH  # sundials includes without "suitesparse/" prefix
    export LIBRARY_PATH=/usr/lib/x86_64-linux-gnu:$LIBRARY_PATH
    export LD_LIBRARY_PATH=/usr/lib/x86_64-linux-gnu:$LD_LIBRARY_PATH
}

if [[ ! -e "$HOME/bin/custom" ]]; then
    mkdir -p "$HOME/bin/custom"
fi
export PATH=$HOME/bin/custom:$PATH


# openblas: make && make install PREFIX=/opt/openblas-0.3.9
# numpy: export OPENBLAS_STATIC_LIB=/opt/openblas-0.3.6/lib/libopenblas.a && BLAS=$OPENBLAS_STATIC_LIB LAPACK=$OPENBLAS_STATIC_LIB setup.py install
# boost: bootstrap.sh && ./b2 install --prefix=/opt/boost_1_72_p --toolset=gcc-9
# symengine: cmake -DCMAKE_INSTALL_PREFIX=/opt/symengine-d0b1932 -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=ON -DWITH_LLVM=ON  ..
# sundials:
#  - msan: CFLAGS="-fsanitize=memory" CC=clang-10 cmake -DCMAKE_INSTALL_PREFIX=/opt/sundials-5.3.0-msan -DCMAKE_BUILD_TYPE=Debug -DBUILD_SHARED_LIBS=ON -DBUILD_STATIC_LIBS=OFF -DEXAMPLES_ENABLE_C=ON -DEXAMPLES_INSTALL=ON -DOPENMP_ENABLE=OFF -DLAPACK_ENABLE:BOOL=OFF -DKLU_ENABLE:BOOL=OFF -DSUNDIALS_INDEX_SIZE=32 ../sundials-5.3.0/
#  - asan: CFLAGS="-fsanitize=address" CC=clang-10 cmake -DCMAKE_INSTALL_PREFIX=/opt/sundials-5.3.0-asan -DCMAKE_BUILD_TYPE=Debug -DBUILD_SHARED_LIBS=ON -DBUILD_STATIC_LIBS=OFF -DEXAMPLES_ENABLE_C=ON -DEXAMPLES_INSTALL=ON -DOPENMP_ENABLE=OFF -DLAPACK_ENABLE:BOOL=OFF -DKLU_ENABLE:BOOL=OFF -DSUNDIALS_INDEX_SIZE=32 ../sundials-5.3.0/ && make -j 8 && ctest && make install
#  - dbg: CFLAGS="-D_FORTIFY_SOURCE=2" CC=gcc-10 cmake -DCMAKE_INSTALL_PREFIX=/opt/sundials-5.3.0-dbg -DCMAKE_BUILD_TYPE=Debug -DBUILD_SHARED_LIBS=ON -DBUILD_STATIC_LIBS=OFF -DEXAMPLES_ENABLE_C=ON -DEXAMPLES_INSTALL=ON -DOPENMP_ENABLE=OFF -DLAPACK_ENABLE:BOOL=OFF -DKLU_ENABLE:BOOL=OFF -DSUNDIALS_INDEX_SIZE=32 ../sundials-5.3.0/ && make -j 8 && ctest && make install
#  - rel: CFLAGS="-march=native -O3 -ffast-math" CC=gcc-10 cmake -DCMAKE_INSTALL_PREFIX=/opt/sundials-5.3.0-rel-klu-lapack -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=ON -DBUILD_STATIC_LIBS=OFF -DEXAMPLES_ENABLE_C=ON -DEXAMPLES_INSTALL=ON -DOPENMP_ENABLE=OFF -DLAPACK_ENABLE:BOOL=ON -DKLU_ENABLE:BOOL=ON -DKLU_INCLUDE_DIR=/usr/include/suitesparse -DKLU_LIBRARY_DIR=/usr/lib/x86_64-linux-gnu -DLAPACK_LIBRARIES=/opt/openblas-0.3.9/lib/libopenblas.so -DSUNDIALS_INDEX_SIZE=32 -DCMAKE_INSTALL_RPATH="/opt/sundials-5.3.0-rel-klu-lapack/lib" -DCMAKE_BUILD_WITH_INSTALL_RPATH=ON ../sundials-5.3.0/
# cpython: curl -Ls https://www.python.org/ftp/python/3.8.1/Python-3.8.1.tar.xz | tar xJ && cd Python-3.8.1 && mkdir build && cd build && CFLAGS="-O2 -march=native" .././configure --prefix=/opt/cpython-3.8 --enable-loadable-sqlite-extensions --enable-shared --with-ensurepip=yes LDFLAGS=-Wl,-rpath=/opt/cpython-3.8/lib && make install && /opt/cpython-3.8/bin/python3 -c "import sqlite3, uuid, lzma, bz2"

if [[ "$(hostname)" == "urania" ]]; then
    export CC=gcc-9 CXX=g++-9 FC=gfortran-9
    export CPLUS_INCLUDE_PATH=/opt/eigen-3.3.7/include
    export SUNDIALS_INSTALL_DIR=/opt/sundials-${SUNDIALS_VERSION:-3.2.1}

    export SYMENGINE_DIR=/opt/symengine-${SYMENGINE_VERSION:-6847e8a}
    if [[ ! -d $SYMENGINE_DIR ]]; then
        >&2 echo "No such directory: $SYMENGINE_DIR"
    fi
    export CMAKE_PREFIX_PATH=$SUNDIALS_INSTALL_DIR:/usr/lib/llvm-8:$SYMENGINE_DIR

    for PREFIX in /opt/openblas-0.3.6 $SUNDIALS_INSTALL_DIR /opt/boost_1_72_p /opt/py36; do
        add_prefix_to_compiler_env_vars $PREFIX
    done
    sundials_fix


    export OPENBLAS_NUM_THREADS=1

    if [[ ! -z ${PY} ]]; then
        CANIDATE_CPY_BIN_DIR="/opt/cpython-$PY/bin"
        if [[ -d "$CANIDATE_CPY_BIN_DIR" ]]; then
	    export PATH="$CANIDATE_CPY_BIN_DIR:$PATH"
        else
            echo "No such directory: $CANIDATE_CPY_BIN_DIR"
        fi
    else
        : # echo "Not using any particular python version"
    fi
elif [[ "$(hostname)" == "yoga720" ]]; then
    export CC=gcc-10 CXX=g++-10 FC=gfortran-10
    export SUNDIALS_ROOT=/opt/sundials-${SUNDIALS_VERSION:-5.3.0-rel-klu-lapack}
    if [[ ! -d $SUNDIALS_ROOT ]]; then
        >&2 echo "No such directory: $SUNDIALS_ROOT"
    fi

    if [[ ! -z ${PY} ]]; then
        CANIDATE_CPY_BIN_DIR="/opt/cpython-$PY/bin"
        if [[ -d "$CANIDATE_CPY_BIN_DIR" ]]; then
	    export PATH="$CANIDATE_CPY_BIN_DIR:$PATH"
        else
            echo "No such directory: $CANIDATE_CPY_BIN_DIR"
        fi
    else
        : # echo "Not using any particular python version"
    fi

    SYMENGINE_DIR=/opt/symengine-${SYMENGINE_VERSION:-14a6bad-rel}
    if [[ ! -d $SYMENGINE_DIR ]]; then
        >&2 echo "No such directory: $SYMENGINE_DIR"
    fi
    for PREFIX in /opt/openblas-0.3.9 /opt/boost_1_72_p $SYMENGINE_DIR $SUNDIALS_ROOT; do
        add_prefix_to_compiler_env_vars $PREFIX
    done
    sundials_fix
    export CMAKE_PREFIX_PATH=$SUNDIALS_INSTALL_DIR:/usr/lib/llvm-10:$SYMENGINE_DIR
    export OPENBLAS_NUM_THREADS=1
    export MPLBACKEND=TkAgg
fi
