#!/bin/bash
LOCATION=/opt/miniconda3
show_help(){
    echo "Usage:"
    echo " -e  environment name"
    echo " -h  show this help"
    echo " -c  set compiler flags"
    echo " -n  set PYTHONNOUSERSITE=1"
    echo " -v  verbose"
    echo ""
    echo "Example:"
    echo ""
    echo "   \$ source $(basename $0)"
    echo "   \$ source $(basename $0) -c -n -e sun3"
}
OPTIND=1
ENV_NAME=""
SET_FLAGS=0
VERBOSE=0
while getopts "h?cve:" opt; do
    case "$opt" in
        h|\?)
            show_help
            exit 0
            ;;
        c)
            SET_FLAGS=1
            ;;
        n)
            export PYTHONNOUSERSITE=1
            ;;
        v)
            VERBOSE=1
            ;;
        e)
            ENV_NAME=$OPTARG
            ;;
    esac
done
shift $((OPTIND-1))

[ "$1" = "--" ] && shift

export PATH=$LOCATION/bin:$PATH
if [[ "$ENV_NAME" == "" ]]; then
    ROOT=$LOCATION
else
    source activate "$ENV_NAME"
    ROOT=$CONDA_PREFIX
fi

if [[ $SET_FLAGS -eq 1 ]]; then
    for DIR in lib include; do
        if [[ ! -d "$ROOT/$DIR" ]]; then
            >&2 echo "Not a directory: $CONDA_PREFIX/$DIR"
            exit 1
        fi
    done
    if [[ $VERBOSE -eq 1 ]]; then
        echo "setting compiler flags..."
    fi
    export CPATH=$ROOT/include:$CPATH
    export LIBRARY_PATH=$ROOT/lib:$LIBRARY_PATH
    export LD_LIBRARY_PATH=$ROOT/lib:$LD_LIBRARY_PATH
fi
if [[ $VERBOSE -eq 1 ]]; then
    echo "conda: $(which conda)"
fi
