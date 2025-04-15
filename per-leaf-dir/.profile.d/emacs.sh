#!/bin/bash
DIR=$(ls -d /opt/emacs-*/ 2>/dev/null | head -n 1)
if [ ! -z "$DIR" ]; then
    export PATH="${DIR%/}/bin:$PATH"
fi
