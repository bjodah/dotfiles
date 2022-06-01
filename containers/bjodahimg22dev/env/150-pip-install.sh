#!/bin/bash -xe
# usage: pass --user flag if running on workstation, and not in docker build.
${PYTHON:-python3} -m pip install $@ --upgrade pip
${PYTHON:-python3} -m pip install $@ \
        ase asv cclib "cython<3" dufte epc flake8 holoviews[recommended] ipykernel ipython ipywidgets ipympl isort joblib jupyter matplotlib mpld3 mogli mypy nbconvert nbsphinx nbstripout "notebook<7" networkx Nikola[extras] numericalunits "numpy<1.22" numpydoc param periodictable ptvsd pulp pudb pybind11 pycparser pycparser-fake-libc pygraphviz pytest-black pytest-cov pytest-docstyle pytest-flake8 pytest-flakes pytest-pep8 pytest-pudb pytest-xdist quantities rstcheck SciencePlots scikit-image scikit-optimize scipy setuptools_scm sphinx sphinx_rtd_theme statsmodels sympy pyfma termplotlib trepan3k toolz virtualenv websockets xarray

${PYTHON:-python3} -m ipykernel install $@
${PYTHON:-python3} -m jupyter nbextension enable $@ --py widgetsnbextension
${PYTHON:-python3} -c "import matplotlib.pyplot as plt"

# bokeh 85M
# plotly 146M
# pandas 59M

# jedi-language-server
# accupy perfplots
