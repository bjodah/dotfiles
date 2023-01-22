#!/bin/bash -e
PREFIX=${1:-/opt/intel-sde}
mkdir -p $PREFIX
#INTEL_SDE_URL=https://software.intel.com/content/dam/develop/external/us/en/documents/downloads/sde-external-8.69.1-2021-07-18-lin.tar.bz2
#INTEL_SDE_URL=https://downloadmirror.intel.com/684899/sde-external-9.0.0-2021-11-07-lin.tar.xz
INTEL_SDE_URL=https://downloadmirror.intel.com/751535/sde-external-9.14.0-2022-10-25-lin.tar.xz
curl -Ls $INTEL_SDE_URL \
    | tar xJ -C /opt/intel-sde --strip-components=1
