add_prefix_to_compiler_env_vars(){
    export CPATH=$1/include:$CPATH
    export LIBRARY_PATH=$1/lib:$LIBRARY_PATH
    export LD_LIBRARY_PATH=$1/lib:$LD_LIBRARY_PATH
}

if [[ ! -e "$HOME/bin/custom" ]]; then
    mkdir -p "$HOME/bin/custom"
fi
export PATH=$HOME/bin/custom:$PATH


if [[ "$(hostname)" == "urania" ]]; then
    export CC=gcc-8 CXX=g++-8 FC=gfortran-8
    export OPENBLAS_NUM_THREADS=1
    for PREFIX in /opt/openblas-0.3.2 /opt/sundials-3.1.2 /opt/boost_1_67_0 /opt/symengine-32d8612 /opt/py36; do
        add_prefix_to_compiler_env_vars $PREFIX
    done
    export CMAKE_PREFIX_PATH=/usr/lib/llvm-6.0:/opt/symengine-9cb1e70
    if [[ ! -e "$HOME/bin/custom/python3.7" ]]; then
	ln -s "$HOME/.local/bin/python3.7" "$HOME/bin/custom/python3.7"
    fi
    if [[ ! -e "$HOME/bin/custom/python3.6" ]]; then
	ln -s "/opt/py36/bin/python3.6" "$HOME/bin/custom/python3.6"
    fi
elif [[ "$(hostname)" == "yoga720" ]]; then
    export CC=gcc-8 CXX=g++-8 FC=gfortran-8
    export OPENBLAS_NUM_THREADS=1
    for PREFIX in /opt/py37 /opt/openblas-0.3.2 /opt/sundials-3.1.2 /opt/boost_1_67_0 /opt/symengine-9cb1e70; do
        add_prefix_to_compiler_env_vars $PREFIX
    done
    export CMAKE_PREFIX_PATH=/usr/lib/llvm-6.0:/opt/symengine-9cb1e70
    if [[ ! -e "$HOME/bin/custom/python3.7" ]]; then
	ln -s "/opt/py37/bin/python3.7" "$HOME/bin/custom/python3.7"
    fi
    # export PATH=/opt/py37/bin:$PATH

    export CPATH=/usr/include/suitesparse:$CPATH  # sundials includes without "suitesparse/" prefix
    export LIBRARY_PATH=/usr/lib/x86_64-linux-gnu:$LIBRARY_PATH
    export LD_LIBRARY_PATH=/usr/lib/x86_64-linux-gnu:$LD_LIBRARY_PATH
fi
