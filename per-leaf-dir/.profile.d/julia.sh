# https://julialang.org/downloads/
DIR=$(ls -d /opt/julia-*/ 2>/dev/null | tail -n 1)
if [[ ! -z "$DIR" ]]; then
    export PATH="${DIR%/}/bin:$PATH"
fi
