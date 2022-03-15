#!/bin/bash -xe
# usage: pass --user flag if running on workstation, and not in docker build.
${PYTHON:-python3} -m pip install $@ --upgrade pip
${PYTHON:-python3} -m pip install $@ \
        appdirs argcomplete argh asv bottle check-manifest CherryPy \
        conan dufte future git-archive-all holoviews[recommended] \
        ipykernel ipython ipywidgets ipympl joblib jupyter \
        mako matplotlib mpld3 nbconvert nbsphinx nbstripout notebook networkx \
        numericalunits numpy param \
        periodictable ptvsd pulp \
        "pygments>=2.4.1" pylatex pyparsing pytest \
        pyyaml quantities \
        SciencePlots scipy setuptools \
        statsmodels sympy termplotlib toolz tqdm \
        wheel wurlitzer xarray

# bokeh 85M
# plotly 146M
# pandas 59M

${PYTHON:-python3} -m pip install --no-use-pep517 sqlalchemy
${PYTHON:-python3} -m ipykernel install $@
${PYTHON:-python3} -m jupyter nbextension enable $@ --py widgetsnbextension
${PYTHON:-python3} -c "import matplotlib.pyplot as plt"
rm -r ~/.cache/pip
