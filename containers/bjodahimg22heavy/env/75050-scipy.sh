#!/bin/bash
set -euxo pipefail
if [ ! -d /opt/scipy-master ]; then
    git clone --recursive https://github.com/scipy/scipy /opt/scipy-master
fi
cd /opt/scipy-master
#git submodule update --init

${PYTHON} -m pip install "numpy>=1.22.4" pytest "cython>=0.29.30" pythran "pybind11>=2.9.2"  # https://docs.scipy.org/doc/scipy/dev/contributor/quickstart_pip.html#building-scipy
${PYTHON} setup.py install
git clean -xfd
cd /
${PYTHON} -c "import scipy"
