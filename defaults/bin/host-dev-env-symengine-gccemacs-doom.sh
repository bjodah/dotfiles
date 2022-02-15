#!/bin/bash
THIS_FOLDER=$(dirname $(realpath $BASH_SOURCE))
THIS_FNAME=$(basename $(realpath $BASH_SOURCE))
if [[ ! -e symengine/basic.h ]]; then
    >&2 echo "Not in a SymEngine source folder?"
fi
if [[ $THIS_IS_RUNNING_IN_CONTAINER == 1 ]]; then
    
else 
    bjodah \
        --container-folder gccemacs-doom \
        --volume $THIS_FOLDER:/opt/my-scripts/ \
        -e THIS_IS_RUNNING_IN_CONTAINER=1 \
        -- /opt/my-scripts/$THIS_FNAME
fi
