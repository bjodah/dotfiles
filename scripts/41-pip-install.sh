#!/bin/bash -x
python3 -m pip install --no-use-pep517 sqlalchemy && \
python3 -m ipykernel install $@ && \
python3 -m jupyter nbextension enable $@ --py widgetsnbextension && \
python3 -c "import matplotlib.pyplot as plt" && \
rm -r ~/.cache/pip
