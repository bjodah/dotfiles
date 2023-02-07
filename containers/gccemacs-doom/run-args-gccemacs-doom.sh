#!/bin/bash

# this file is sourced by `run-container`
CUR_DIR=$(dirname $(realpath $BASH_SOURCE))
PODMAN_RUN_ARGS+=("--volume $(realpath $CUR_DIR/env/.doom.d):/root/.doom.d")

