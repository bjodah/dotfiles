#!/bin/bash
LOCATION=/opt/miniconda3
export PATH=$LOCATION/bin:$PATH
echo $@
if [ "$#" -eq 0 ]; then
    ROOT=$LOCATION
elif [ "$#" -eq 1 ]; then
    source activate $1
    ROOT=$CONDA_PREFIX
else
    >&2 echo "Expected 0 or 1 argument"
fi
export CPLUS_INCLUDE_PATH=$ROOT/include:$CPLUS_INCLUDE_PATH
export C_INCLUDE_PATH=$ROOT/include:$C_INCLUDE_PATH
export LIBRARY_PATH=$ROOT/lib:$LIBRARY_PATH
export LD_LIBRARY_PATH=$ROOT/lib:$LD_LIBRARY_PATH
