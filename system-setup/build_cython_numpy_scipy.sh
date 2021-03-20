#!/bin/bash
set -euxo pipefail
$CC --version
$CXX --version
$FC --version
PYTHON=${PYTHON:-python3}
$PYTHON --version
for pypkg in cython numpy scipy; do
    if $PYTHON -c "import cython"; then
        2>&1 echo "cython already installed"
        exit 1
    fi
done
OPENBLAS_STATIC_LIB=/opt/openblas-0.3.6/lib/libopenblas.a
if [ ! -e "$OPENBLAS_STATIC_LIB" ]; then >&2 echo "Missing openblas static lib: $OPENBLAS_STATIC_LIB"; fi
cd /dev/shm
export BLAS=$OPENBLAS_STATIC_LIB
export LAPACK=$OPENBLAS_STATIC_LIB
for pypkg in pybind11 cython numpy scipy; do
    if [[ "$pypkg" == pybind11 ]]; then
        URL_BASE=https://github.com/pybind/pybind11
    else
        URL_BASE=https://github.com/$pypkg/$pypkg
    fi
    if [ ! -d ./$pypkg-master ]; then curl -Ls $URL_BASE/archive/master.tar.gz | tar xz; fi
    cd $pypkg-master; $PYTHON setup.py install; cd -; rm -r $pypkg-master
    if ! $PYTHON -c "import $pypkg"; then
        >&2 echo "Exiting, failed to install: $pypkg"
        exit 1
    fi
done
