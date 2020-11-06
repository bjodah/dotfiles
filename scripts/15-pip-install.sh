#!/bin/bash -xe
# usage: pass --user flag if running on workstation, and not in docker build.
python3 -m pip install \
        appdirs argh ase asv black bokeh bottle cclib check-manifest CherryPy \
        cython epc flake8 future git-archive-all holoviews[recommended] \
        ipykernel ipython ipywidgets isort jedi-language-server joblib jupyter \
        mako matplotlib mogli mpld3 mypy nbconvert nbsphinx networkx \
        Nikola[extras] numericalunits numpy numpydoc pandas param \
        periodictable plotly pudb pulp pybind11 pycparser pycparser-fake-libc \
        "pygments>=2.4.1" pygraphviz pylatex pyparsing pytest pytest-cov \
        pytest-docstyle pytest-flake8 pytest-flakes pytest-pep8 pytest-pudb \
        pytest-xdist python-language-server[all] quantities rstcheck \
        scikit-image scikit-optimize scipy setuptools setuptools_scm sphinx \
        sphinx_rtd_theme statsmodels sympy termplotlib toolz tqdm trepan3k \
        virtualenv wheel wurlitzer xarray

python3 -m pip install --no-use-pep517 sqlalchemy
python3 -m ipykernel install $@
python3 -m jupyter nbextension enable $@ --py widgetsnbextension
python3 -c "import matplotlib.pyplot as plt"
rm -r ~/.cache/pip
