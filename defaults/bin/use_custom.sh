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


# openblas: make install PREFIX=/opt/openblas-0.3.4
# numpy: export OPENBLAS_STATIC_LIB=/opt/openblas-0.3.6/lib/libopenblas.a && BLAS=$OPENBLAS_STATIC_LIB LAPACK=$OPENBLAS_STATIC_LIB setup.py install
# boost: bootstrap.sh && ./b2 install --prefix=/opt/boost_1_68_0 --toolset=gcc-8
# symengine: cmake -DCMAKE_INSTALL_PREFIX=/opt/symengine-7f13827 -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=ON -DWITH_LLVM=ON  ..
# sundials: cmake -DCMAKE_INSTALL_PREFIX=/opt/sundials-3.2.1 -DCMAKE_BUILD_TYPE=Release -DBUILD_SHARED_LIBS=ON -DBUILD_STATIC_LIBS=OFF -DEXAMPLES_ENABLE_C=OFF -DEXAMPLES_INSTALL=OFF -DOPENMP_ENABLE=OFF -DLAPACK_ENABLE:BOOL=ON -DKLU_ENABLE:BOOL=OFF -DSUNDIALS_INDEX_SIZE=32 -DKLU_ENABLE=ON -DKLU_INCLUDE_DIR=/usr/include/suitesparse -DKLU_LIBRARY_DIR=/usr/lib/x86_64-linux-gnu ..
# cpython: curl -Ls https://www.python.org/ftp/python/3.7.3/Python-3.7.3.tar.xz | tar xJ && cd Python-3.7.3 && mkdir build && cd build && CFLAGS="-O2 -march=native" ../configure --prefix=/opt/cpython-3.7.3 --enable-loadable-sqlite-extensions --enable-shared --with-ensurepip=yes && LD_LIBRARY_PATH=$(pwd) ./python -c "import sqlite3, uuid, lzma, bz2" && make install

if [[ "$(hostname)" == "urania" ]]; then
    export CC=gcc-8 CXX=g++-8 FC=gfortran-8
    export CPLUS_INCLUDE_PATH=/opt/eigen-3.3.7/include
    export SUNDIALS_INSTALL_DIR=/opt/sundials-${SUNDIALS_VERSION:-3.2.1}
    for PREFIX in /opt/openblas-0.3.4 $SUNDIALS_INSTALL_DIR /opt/boost_1_68_0 /opt/symengine-6847e8a /opt/py36; do
        add_prefix_to_compiler_env_vars $PREFIX
    done
    sundials_fix
    export CMAKE_PREFIX_PATH=$SUNDIALS_INSTALL_DIR:/usr/lib/llvm-6.0:/opt/symengine-6847e8a

    export OPENBLAS_NUM_THREADS=1

    if [[ ${PY:-3.5} == "3.6" ]]; then
        export PATH=/opt/py36/bin:$PATH
    elif [[ ${PY:-3.5} == "3.7" ]]; then
	export PATH=/opt/cpython-3.7/bin:$PATH
	#export LD_LIBRARY_PATH=/opt/cpython-3.7.3/lib:$LD_LIBRARY_PATH
    elif [[ ${PY:-3.5} == "master" ]]; then
	export PATH=/opt/cpython-master/bin:$PATH
	#export LD_LIBRARY_PATH=/opt/cpython-3.7.3/lib:$LD_LIBRARY_PATH
    else
        echo "Got \$PY=$PY, using python3.5"
    fi
elif [[ "$(hostname)" == "yoga720" ]]; then
    export CC=gcc-8 CXX=g++-8 FC=gfortran-8
    export SUNDIALS_INSTALL_DIR=/opt/sundials-${SUNDIALS_VERSION:-3.2.1}

    if [[ ! -z ${PY} ]]; then
        CANIDATE_CPY_BIN_DIR="/opt/cpython-$PY/bin"
        if [[ -d "$CANIDATE_CPY_BIN_DIR" ]]; then
	    export PATH="$CANIDATE_CPY_BIN_DIR:$PATH"
        else
            echo "No such directory: $CANIDATE_CPY_BIN_DIR"
        fi
    else
        echo "Not using any particular python version"
    fi

    for PREFIX in /opt/py37 /opt/openblas-master /opt/boost_1_70_0 /opt/symengine-ab7c16a $SUNDIALS_INSTALL_DIR; do
        add_prefix_to_compiler_env_vars $PREFIX
    done
    sundials_fix
    export CMAKE_PREFIX_PATH=$SUNDIALS_INSTALL_DIR:/usr/lib/llvm-8:/opt/symengine-ab7c16a

    export OPENBLAS_NUM_THREADS=1

    export MPLBACKEND=TkAgg
fi
