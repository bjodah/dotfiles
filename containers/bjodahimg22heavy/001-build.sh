#!/bin/bash
set -euxo pipefail


OPT_FOLDER=opt2

for d in build env/$OPT_FOLDER; do
    if [ ! -d ./$d ]; then
        mkdir $d
    fi
done

export CC=gcc-12 CXX=g++-12
IMAGE=$(grep FROM ./env/Containerfile | head -n 1 | cut -d' ' -f2)

build() {
    podrun -v ./build:/build -v ./env/$OPT_FOLDER/:/$OPT_FOLDER -v ~/.ccache:/root/.ccache --image $IMAGE -e CXX -e CC $@
}


if [ ! -e env/$OPT_FOLDER/cargo ]; then
    build -e CARGO_HOME=/$OPT_FOLDER/cargo -e RUSTUP_HOME=/$OPT_FOLDER/rustup -- "bash 67000-rust.sh && . /$OPT_FOLDER/cargo/env && cargo install difftastic"
fi


export BOOST_MAJOR=1 BOOST_MINOR=81 BOOST_PATCH=0
BOOST_DIR=boost-${BOOST_MAJOR}.${BOOST_MINOR}.${BOOST_PATCH}
if [ ! -e env/$OPT_FOLDER/$BOOST_DIR/ ]; then
    build -e BOOST_MAJOR=1 -e BOOST_MINOR=81 -e BOOST_PATCH=0 -- bash ./750-boost.sh /$OPT_FOLDER
fi

for VERSION_VARIANT in 3.11-release v3.12.0a4-debug; do
    export CPYTHON_VERSION=${VERSION_VARIANT%%-*}
    export CPYTHON_VARIANT=${VERSION_VARIANT##*-}
    CPYTHON_DIR=cpython-${CPYTHON_VERSION}-${CPYTHON_VARIANT}
    mkdir -p ~/.cache/pip
    if [ ! -d env/$OPT_FOLDER/$CPYTHON_DIR ]; then
        build -v ~/.cache/pip:/root/.cache/pip -e CPYTHON_VERSION -e CPYTHON_VARIANT -- "\
bash 75000-cpython.sh /$OPT_FOLDER"
    fi
    if ! build -- "/$OPT_FOLDER/$CPYTHON_DIR/bin/python3 -c 'import scipy'"; then
        cp ../bjodahimg22base/env/15-pip-install.sh ./build/
        if [[ $CPYTHON_VERSION == *3.12* ]]; then
            cp 75050-scipy.sh ./build
#/$OPT_FOLDER/$CPYTHON_DIR/bin/python3 -m pip uninstall cython && \
            build -v ~/.cache/pip:/root/.cache/pip -e CPYTHON_VERSION -e CPYTHON_VARIANT -- "\
/$OPT_FOLDER/$CPYTHON_DIR/bin/python3 -m pip install https://github.com/cython/cython/archive/master.tar.gz \
&& env PATH=/$OPT_FOLDER/$CPYTHON_DIR/bin:$PATH PYTHON=/$OPT_FOLDER/$CPYTHON_DIR/bin/python3 CC='ccache $CC' CXX='ccache $CXX' bash /build/75050-scipy.sh"
        else
            build -v ~/.cache/pip:/root/.cache/pip -e CPYTHON_VERSION -e CPYTHON_VARIANT -- "/$OPT_FOLDER/$CPYTHON_DIR/bin/python3 -m pip install scipy"
        fi
        if ! build -- "/$OPT_FOLDER/$CPYTHON_DIR/bin/python3 -c 'import scipy'"; then
            >&2 echo "Failed to install scipy?"
            exit 1
        fi
    fi    
    if ! build -- "/$OPT_FOLDER/$CPYTHON_DIR/bin/python3 -c 'import mpmath'"; then
        build -v ~/.cache/pip:/root/.cache/pip -- "env PYTHON=/$OPT_FOLDER/$CPYTHON_DIR/bin/python3 CC='ccache $CC' CXX='ccache $CXX' bash /build/15-pip-install.sh"
    fi
    if ! build -- "/$OPT_FOLDER/$CPYTHON_DIR/bin/python3 -c 'import jupyter'"; then
        sed -e 's/"numba<0.57"//g' -e 's/trepan3k//g' ../bjodahimg22dev/env/150-pip-install.sh >./build/150-pip-install.sh
        if [[ $CPYTHON_VERSION == *3.12* ]]; then
            sed -i \
                -e 's/pylatex//g' \
                -e 's@"scipy>=1.9.3"@@g' \
                -e 's/statsmodels//g' \
                -e 's/scikit-optimize//g' \
                -e 's/"cython<3"/"cython>=3.0.0a11"/g' \
                -e 's/"numpy<1.24"/"numpy>=1.24.1"/g' \
                ./build/150-pip-install.sh
        fi
        build -v ~/.cache/pip:/root/.cache/pip -- "env PYTHON=/$OPT_FOLDER/$CPYTHON_DIR/bin/python3 CC='ccache $CC' CXX='ccache $CXX' bash /build/150-pip-install.sh"
fi
done

SYMENGINE_COMMIT=fcef5c7d6cc848e3f6c0b9ecc5a22d30e5e98f99
SYMENGINE_VERSION=${SYMENGINE_COMMIT:0:7}
for SYMENGINE_VARIANT in release debug glibcxxdbg asan msan; do  # tcmalloc
    SYMENGINE_DIR=symengine-${SYMENGINE_VERSION}-${SYMENGINE_VARIANT}
    if [ ! -e env/$OPT_FOLDER/$SYMENGINE_DIR ]; then
        build \
            -e SYMENGINE_COMMIT=$SYMENGINE_COMMIT \
            -e SYMENGINE_VERSION=$SYMENGINE_VERSION \
            -e SYMENGINE_VARIANT=$SYMENGINE_VARIANT \
            -e BOOST_DIR=$BOOST_DIR \
            -e OPT_FOLDER=$OPT_FOLDER -- bash 87000-get_symengine.sh /$OPT_FOLDER
    fi
done

JULIA_MAJOR=1
JULIA_MINOR=8
JULIA_PATCH=4
JL_V2=$JULIA_MAJOR.$JULIA_MINOR
JL_V3=$JULIA_MAJOR.$JULIA_MINOR.$JULIA_PATCH
JULIA_DIR=julia-$JL_V3
if [ ! -e env/$OPT_FOLDER/$JULIA_DIR ]; then
    build -e JL_V2=$JL_V2 -e JL_V3=$JL_V3 -- bash 85000-julia.sh /$OPT_FOLDER
fi

#89000-build-ipopt.sh 

PYPY39_VERSION=7.3.11
if ! compgen -G "env/$OPT_FOLDER/pypy3.9*" >/dev/null; then
    build -v ~/.cache/pip:/root/.cache/pip -e PYPY39_VERSION=$PYPY39_VERSION -- bash 25000-install-pypy.sh /$OPT_FOLDER
fi

( ulimit -n 4096; podman build env/ )
