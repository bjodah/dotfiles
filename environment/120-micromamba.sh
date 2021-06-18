#!/bin/bash
curl -fsSLo miniforge3.sh https://github.com/conda-forge/miniforge/releases/latest/download/Miniforge3-$(uname)-$(uname -m).sh
bash miniforge3.sh -b
~/miniforge3/bin/mamba create -n xeus-python
~/miniforge3/bin/source activate xeus-python
mamba install xeus-python notebook -c conda-forge
