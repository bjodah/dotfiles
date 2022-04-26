#!/bin/bash
set -euxo pipefail
curl -Ls https://downloads.python.org/pypy/pypy3.9-v7.3.8-linux64.tar.bz2 | tar xj -C /opt
/opt/pypy3.9*/bin/pypy3.9 -m ensurepip
/opt/pypy3.9*/bin/pypy3.9 -m pip install cppyy ipython
/opt/pypy3.9*/bin/pypy3.9 -m IPython -c "import cppyy; import IPython; print(IPython.__version__)"
