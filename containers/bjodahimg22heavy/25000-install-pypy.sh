#!/bin/bash
set -euxo pipefail
PREFIX=$1
if [ ! -d $PREFIX ]; then
    >&2 echo "Non-existent PREFIX (first arg): $PREFIX"
fi

PYPY39_URL=https://downloads.python.org/pypy/pypy3.9-v${PYPY39_VERSION}-linux64.tar.bz2
curl -Ls $PYPY39_URL | tar xj -C $PREFIX
$PREFIX/pypy3.9*/bin/pypy3.9 -m ensurepip
$PREFIX/pypy3.9*/bin/pypy3.9 -m pip install cppyy ipython
$PREFIX/pypy3.9*/bin/pypy3.9 -m IPython -c "import cppyy; import IPython; print(IPython.__version__)"
