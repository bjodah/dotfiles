# export CMAKE_GENERATOR=Ninja
# export CONAN_CMAKE_GENERATOR=Ninja

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
# numpy: export OPENBLAS_STATIC_LIB=/opt/openblas-0.3.9/lib/libopenblas.a && BLAS=$OPENBLAS_STATIC_LIB LAPACK=$OPENBLAS_STATIC_LIB setup.py install
# boost: bootstrap.sh && ./b2 install --prefix=/opt/boost_1_73_p --toolset=gcc-10
# symengine:
#  - asan: CXXFLAGS="-fsanitize=address" CXX=clang++-11 CC=clang-11 cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_INSTALL_PREFIX=/opt/symengine-46090cf-asan -DCMAKE_BUILD_TYPE=Debug -DBUILD_SHARED_LIBS=ON -DWITH_COTIRE=OFF -DWITH_LLVM=ON ~/vc/symengine
#  - dbg: CXXFLAGS="-D_FORTIFY_SOURCE=2 -D_GLIBCXX_DEBUG -D_GLIBCXX_DEBUG_PEDANTIC" CC=gcc-10 CXX=g++-10 cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_INSTALL_PREFIX=/opt/symengine-46090cf-dbg -DCMAKE_BUILD_TYPE=Debug -DBUILD_SHARED_LIBS=ON -DWITH_LLVM=OFF ~/vc/symengine
# sundials:
#  - msan: CFLAGS="-fsanitize=memory" CC=clang-11 cmake -DCMAKE_INSTALL_PREFIX=/opt/sundials-5.4.0-msan -DCMAKE_BUILD_TYPE=Debug -DBUILD_SHARED_LIBS=ON -DBUILD_STATIC_LIBS=OFF -DEXAMPLES_ENABLE_C=ON -DEXAMPLES_INSTALL=ON -DOPENMP_ENABLE=OFF -DLAPACK_ENABLE:BOOL=OFF -DKLU_ENABLE:BOOL=OFF -DSUNDIALS_INDEX_SIZE=32 ../sundials-5.4.0/
#  - asan: CFLAGS="-fsanitize=address" CC=clang-11 cmake -DCMAKE_INSTALL_PREFIX=/opt/sundials-5.4.0-asan -DCMAKE_BUILD_TYPE=Debug -DBUILD_SHARED_LIBS=ON -DBUILD_STATIC_LIBS=OFF -DEXAMPLES_ENABLE_C=ON -DEXAMPLES_INSTALL=ON -DOPENMP_ENABLE=OFF -DLAPACK_ENABLE:BOOL=OFF -DKLU_ENABLE:BOOL=OFF -DSUNDIALS_INDEX_SIZE=32 ../sundials-5.4.0/ && make -j 8 && ctest && make install
#  - dbg: CFLAGS="-D_FORTIFY_SOURCE=2" CC=gcc-10 cmake -DCMAKE_INSTALL_PREFIX=/opt/sundials-5.4.0-dbg -DCMAKE_BUILD_TYPE=Debug -DBUILD_SHARED_LIBS=ON -DBUILD_STATIC_LIBS=OFF -DEXAMPLES_ENABLE_C=ON -DEXAMPLES_INSTALL=ON -DOPENMP_ENABLE=OFF -DLAPACK_ENABLE:BOOL=OFF -DKLU_ENABLE:BOOL=OFF -DSUNDIALS_INDEX_SIZE=32 ../sundials-5.4.0/ && make -j 8 && ctest && make install
#  - rel: CFLAGS="-march=native -O3 -ffast-math" LDFLAGS=-Wl,-rpath-link,/opt/openblas-0.3.11pre/lib CC=gcc-10 cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_INSTALL_PREFIX=/opt/sundials-5.4.0-klu-openblas -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=ON -DBUILD_STATIC_LIBS=OFF -DEXAMPLES_ENABLE_C=ON -DEXAMPLES_INSTALL=ON -DOPENMP_ENABLE=OFF -DLAPACK_ENABLE:BOOL=ON -DKLU_ENABLE:BOOL=ON -DKLU_INCLUDE_DIR=/usr/include/suitesparse -DKLU_LIBRARY_DIR=/usr/lib/x86_64-linux-gnu -DLAPACK_LIBRARIES=/opt/openblas-0.3.11pre/lib/libopenblas.so -DSUNDIALS_INDEX_SIZE=32 -DCMAKE_INSTALL_RPATH="/opt/sundials-5.4.0-klu-openblas/lib;/opt/openblas-0.3.11pre/lib" -DCMAKE_BUILD_WITH_INSTALL_RPATH=ON ../sundials-5.4.0/
# cpython: curl -Ls https://www.python.org/ftp/python/3.8.1/Python-3.8.1.tar.xz | tar xJ && cd Python-3.8.1 && mkdir build && cd build && CFLAGS="-O2 -march=native" .././configure --prefix=/opt/cpython-3.8 --enable-loadable-sqlite-extensions --enable-shared --with-ensurepip=yes LDFLAGS=-Wl,-rpath=/opt/cpython-3.8/lib && make install && /opt/cpython-3.8/bin/python3 -c "import sqlite3, uuid, lzma, bz2"
# ccls:
#  $ git clone --depth=1 --recursive https://github.com/MaskRay/ccls && mkdir ccls/build && cd ccls/build && cmake -DCMAKE_BUILD_TYPE=Release -DCLANG_RESOURCE_DIR=$(clang-11 -print-resource-dir) -DCMAKE_INSTALL_PREFIX=/opt/ccls .. && make -j 8 && make install

# pycvodes2: CC=gcc-10 PYCVODES2_LAPACK=openblas CFLAGS="-O3 -march=native -isystem /usr/include/suitesparse -isystem /opt/sundials-5.4.0-klu-openblas/include" LDFLAGS="-Wl,--disable-new-dtags -Wl,-rpath,/opt/sundials-5.4.0-klu-openblas/lib -L/opt/sundials-5.4.0-klu-openblas/lib" python3 setup.py develop --user
if [[ "$(hostname)" == "urania" ]]; then
    export CC=gcc-9 CXX=g++-9 FC=gfortran-9
    export CPLUS_INCLUDE_PATH=/opt/eigen-3.3.7/include
    export SUNDIALS_INSTALL_DIR=/opt/sundials-${SUNDIALS_VERSION:-5.3.0-rel-klu-lapack}

    export SYMENGINE_DIR=/opt/symengine-${SYMENGINE_VERSION:-0e3b6f5-rel}
    if [[ ! -d $SYMENGINE_DIR ]]; then
        >&2 echo "No such directory: $SYMENGINE_DIR"
    fi
    export CMAKE_PREFIX_PATH=$SUNDIALS_INSTALL_DIR:/usr/lib/llvm-8:$SYMENGINE_DIR

    for PREFIX in /opt/openblas-0.3.9 $SUNDIALS_INSTALL_DIR /opt/boost_1_73_p /opt/py36; do
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
    export BOOST_ROOT=/opt/boost-1.74.p
    export CXXFLAGS="-isystem ${BOOST_ROOT}/include $CXXFLAGS"
    export SUNDIALS_ROOT=/opt/sundials-${SUNDIALS_VERSION:-5.5.0-debug}
    if [[ ! -d $SUNDIALS_ROOT ]]; then
        >&2 echo "No such directory: $SUNDIALS_ROOT"
    fi
    export PYODESYS_CVODE_FLAGS="-isystem ${SUNDIALS_ROOT}/include -isystem /usr/include/suitesparse" \
    export PYODESYS_CVODE_LDFLAGS="-Wl,--disable-new-dtags -Wl,-rpath,${SUNDIALS_ROOT}/lib -L${SUNDIALS_ROOT}/lib"

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

    SYMENGINE_DIR=/opt/symengine-${SYMENGINE_VERSION:-46090cf-dbg}
    if [[ ! -d $SYMENGINE_DIR ]]; then
        >&2 echo "No such directory: $SYMENGINE_DIR"
    else
        export CMAKE_PREFIX_PATH=$SYMENGINE_DIR:$CMAKE_PREFIX_PATH
    fi

    # for PREFIX in /opt/openblas-0.3.11pre /opt/boost_1_74_p $SUNDIALS_ROOT; do
    #     add_prefix_to_compiler_env_vars $PREFIX
    # done
    # sundials_fix
    export CMAKE_PREFIX_PATH=$SUNDIALS_ROOT:/usr/lib/llvm-11:$SYMENGINE_DIR
    export OPENBLAS_NUM_THREADS=1
    export MPLBACKEND=GTK3Agg
fi

