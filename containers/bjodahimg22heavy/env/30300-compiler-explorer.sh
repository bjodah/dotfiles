#!/bin/bash
OPT_DIR=$1
if [ ! -e $OPT_DIR ]; then
    mkdir $OPT_DIR
fi
git clone --depth 1 https://github.com/compiler-explorer/compiler-explorer $OPT_DIR/compiler-explorer
make -C $OPT_DIR/compiler-explorer test-min
