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

    if [[ ${PY:-3.6} == "3.7" ]]; then
	export PATH=/opt/cpython-3.7.3/bin:$PATH
	export LD_LIBRARY_PATH=/opt/cpython-3.7.3/lib:$LD_LIBRARY_PATH
    else
	if [[ ! -e "$HOME/bin/custom/python3.6" ]]; then
	    ln -s "/opt/py36/bin/python3.6" "$HOME/bin/custom/python3.6"
	fi
    fi
elif [[ "$(hostname)" == "yoga720" ]]; then
    export CC=gcc-8 CXX=g++-8 FC=gfortran-8
    export OPENBLAS_NUM_THREADS=1
    for PREFIX in /opt/py37 /opt/openblas-0.3.4 /opt/boost_1_68_0 /opt/symengine-af64b72c  /opt/sundials-${SUNDIALS_VERSION:-3.2.1}; do
        add_prefix_to_compiler_env_vars $PREFIX
    done
    export CMAKE_PREFIX_PATH=/usr/lib/llvm-6.0:/opt/symengine-af64b72c
    if [[ ! -e "$HOME/bin/custom/python3.7" ]]; then
	ln -s "/opt/py37/bin/python3.7" "$HOME/bin/custom/python3.7"
    fi
    # export PATH=/opt/py37/bin:$PATH
    export MPLBACKEND=TkAgg
    sundials_fix
fi
