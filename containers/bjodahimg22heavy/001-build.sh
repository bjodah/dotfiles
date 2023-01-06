#!/bin/bash
set -euxo pipefail


OPT_FOLDER=opt2

for d in build env/$OPT_FOLDER; do
    if [ ! -d ./$d ]; then
        mkdir $d
    fi
done

# I give up, (GNU) make is just *terrible*, e.g. https://stackoverflow.com/a/26518222/790973
#make -f _Makefile
# ..instead, let's just stick to bash 
IMAGE=$(grep FROM ./env/Containerfile | head -n 1 | cut -d' ' -f2)

build() {
    podrun -v ./build:/build -v ./env/$OPT_FOLDER/:/$OPT_FOLDER -v ~/.ccache:/root/.ccache --image $IMAGE -e CXX -e CC $@
}


if [ ! -e env/$OPT_FOLDER/cargo ]; then
    build -e CARGO_HOME=/$OPT_FOLDER/cargo -e RUSTUP_HOME=/$OPT_FOLDER/rustup -- "bash 67000-rust.sh && . /$OPT_FOLDER/cargo/env && env CC=gcc-12 CXX=g++-12 cargo install difftastic"
fi


export BOOST_MAJOR=1 BOOST_MINOR=81 BOOST_PATCH=0
BOOST_DIR=boost-${BOOST_MAJOR}.${BOOST_MINOR}.${BOOST_PATCH}
if [ ! -e env/$OPT_FOLDER/$BOOST_DIR/ ]; then
    build -e BOOST_MAJOR=1 -e BOOST_MINOR=81 -e BOOST_PATCH=0 -- bash ./750-boost.sh /$OPT_FOLDER
fi

export CPYTHON_VERSION=3.11
export CPYTHON_VARIANT=release
CPYTHON_DIR=cpython-3.11-release
if [ ! -e env/$OPT_FOLDER/$CPYTHON_DIR ]; then
    cp ../bjodahimg22base/env/15-pip-install.sh ./build/
    build -e CPYTHON_VERSION -e CPYTHON_VARIANT -- "\
bash 75000-cpython.sh /$OPT_FOLDER \
&& env PYTHON=/$OPT_FOLDER/$CPYTHON_DIR/bin/python3 bash /build/15-pip-install.sh"
fi

SYMENGINE_COMMIT=fcef5c7d6cc848e3f6c0b9ecc5a22d30e5e98f99
SYMENGINE_VERSION=${SYMENGINE_COMMIT:0:7}
for SYMENGINE_VARIANT in release debug msan; do  # tcmalloc
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


#89000-build-ipopt.sh 

