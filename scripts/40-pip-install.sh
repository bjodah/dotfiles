#!/bin/bash
python3 -m pip install setuptools wheel virtualenv future appdirs pybind11 cython pycparser pycparser-fake-libc \
            flake8 pudb pytest pytest-pep8 pytest-cov pytest-flakes pytest-flake8 pytest-xdist pytest-docstyle pytest-pudb mypy black isort \
            numpy numpydoc scipy statsmodels sympy pandas matplotlib mpld3 jupyter ipython ipykernel \
            scikit-optimize joblib toolz param quantities pylatex bokeh mogli xarray periodictable ase numericalunits \
            tqdm pyparsing ipywidgets pulp holoviews[recommended] cclib scikit-image git-archive-all wurlitzer \
            sphinx sphinx_rtd_theme argh rstcheck check-manifest setuptools_scm mako bottle CherryPy Nikola[extras] nbsphinx \
            networkx pygraphviz plotly nbconvert asv trepan3k epc python-language-server[all] jedi-language-server termplotlib "pygments>=2.4.1" && \
