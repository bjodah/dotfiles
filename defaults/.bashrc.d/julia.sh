# https://julialang.org/downloads/
DIR=$(ls -d /opt/julia-*/ | tail -n 1)
if [ -d "$DIR" ]; then
    export PATH="${DIR%/}/bin:$PATH"
fi
