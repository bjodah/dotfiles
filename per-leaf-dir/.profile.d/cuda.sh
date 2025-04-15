#!/bin/bash
CUDA_DIR=/usr/local/cuda
if [ -e "$CUDA_DIR" ]; then
    #https://askubuntu.com/a/1023292/143454
    export CUDA_HOME="$CUDA_DIR"
    export LD_LIBRARY_PATH="$CUDA_DIR/lib64:$CUDA_DIR/extras/CUPTI/lib64:$LD_LIBRARY_PATH"
    export PATH="$CUDA_HOME/bin:$PATH"
    if ! which nvcc >/dev/null 2>&1; then
	>&2 echo "warning: no nvcc in \$CUDA_HOME/bin?"
    fi
fi
