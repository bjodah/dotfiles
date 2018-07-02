if [[ "$(hostname)" == "urania" ]]; then
    export CC=gcc-8 CXX=g++-8 FC=gfortran-8 CPATH=$HOME/.local/include:/opt/sundials-3.1.1-0.3.1/include:/opt/openblas/include:/opt/boost_1_67_0/include LIBRARY_PATH=$HOME/.local/lib:/opt/sundials-3.1.1-0.3.1/lib:/opt/openblas-0.3.1/lib:/opt/boost_1_67_0/lib LD_LIBRARY_PATH=$HOME/.local/lib:/opt/sundials-3.1.1-0.3.1/lib:/opt/openblas-0.3.1/lib:/opt/boost_1_67_0/lib OPENBLAS_NUM_THREADS=1
    if [[ ! -e "$HOME/bin/custom" ]]; then
	mkdir -p "$HOME/bin/custom"
    fi
    export PATH=$HOME/bin/custom:$PATH
    if [[ ! -e "$HOME/bin/custom/python3" ]]; then
	ln -s "$HOME/.local/bin/python3.7" "$HOME/bin/custom/python3"
    fi
fi
