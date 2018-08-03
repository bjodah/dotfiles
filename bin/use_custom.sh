if [[ "$(hostname)" == "urania" ]]; then
    export CC=gcc-8 CXX=g++-8 FC=gfortran-8 OPENBLAS_NUM_THREADS=1
    export CPATH=$HOME/.local/include:/opt/sundials-3.1.1-0.3.1/include:/opt/openblas/include:/opt/boost_1_67_0/include:/opt/symengine-32d8612/include:/usr/include/suitesparse
    export LIBRARY_PATH=$HOME/.local/lib:/opt/sundials-3.1.1-0.3.1/lib:/opt/openblas-0.3.1/lib:/opt/boost_1_67_0/lib:/opt/symengine-32d8612/lib
    export LD_LIBRARY_PATH=$LIBRARY_PATH:/opt/py36/lib
    export CMAKE_PREFIX_PATH=/opt/symengine-32d8612
    if [[ ! -e "$HOME/bin/custom" ]]; then
	mkdir -p "$HOME/bin/custom"
    fi
    export PATH=$HOME/bin/custom:$PATH
    if [[ ! -e "$HOME/bin/custom/python3.7" ]]; then
	ln -s "$HOME/.local/bin/python3.7" "$HOME/bin/custom/python3.7"
    fi
    if [[ ! -e "$HOME/bin/custom/python3.6" ]]; then
	ln -s "/opt/py36/bin/python3.6" "$HOME/bin/custom/python3.6"
    fi
elif [[ "$(hostname)" == "yoga720" ]]; then
    export CC=gcc-8 CXX=g++-8 FC=gfortran-8
    export CPATH=$HOME/.local/include:/opt/sundials-3.1.1/include:/opt/py37/include:/opt/boost_1_67_0/include
    export LIBRARY_PATH=/opt/sundials-3.1.1/lib:/opt/py37/lib:/opt/boost_1_67_0/lib
    export LD_LIBRARY_PATH=$LIBRARY_PATH
    export PATH=/opt/py37/bin:$PATH
fi
