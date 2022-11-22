#!/bin/bash -xe
# usage: pass --user flag if running on workstation, and not in docker build.
${PYTHON:-python3} -m pip install $@ \
                   bokeh plotly pandas cppyy xrviz numba>=0.55.1 uqpy CherryPy bottle sqlalchemy cclib accupy perfplots
