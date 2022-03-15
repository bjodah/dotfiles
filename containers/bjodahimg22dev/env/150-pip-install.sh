#!/bin/bash -xe
# usage: pass --user flag if running on workstation, and not in docker build.
${PYTHON:-python3} -m pip install $@ --upgrade pip
${PYTHON:-python3} -m pip install $@ \
        ase asv black cclib cython epc flake8  isort mogli mypy Nikola[extras] numpydoc pudb pybind11 pycparser pycparser-fake-libc pygraphviz pytest-black pytest-cov pytest-docstyle pytest-flake8 pytest-flakes pytest-pep8 pytest-pudb pytest-xdist rstcheck scikit-image scikit-optimize setuptools_scm sphinx sphinx_rtd_theme trepan3k virtualenv websockets

# jedi-language-server
# accupy perfplots
