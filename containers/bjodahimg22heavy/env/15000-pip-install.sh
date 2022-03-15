#!/bin/bash -xe
# usage: pass --user flag if running on workstation, and not in docker build.
${PYTHON:-python3} -m pip install $@ --upgrade pip
${PYTHON:-python3} -m pip install $@ \
                   bokeh plotly pandas

${PYTHON:-python3} -m pip install --no-use-pep517 sqlalchemy
${PYTHON:-python3} -m ipykernel install $@
${PYTHON:-python3} -m jupyter nbextension enable $@ --py widgetsnbextension
${PYTHON:-python3} -c "import matplotlib.pyplot as plt"
rm -r ~/.cache/pip
