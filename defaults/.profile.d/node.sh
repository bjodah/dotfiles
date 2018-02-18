# https://nodejs.org/en/download/
DIR=$(ls -d /opt/node-v*/ 2>/dev/null | tail -n 1)
if [[ ! -z "$DIR" ]]; then
    export PATH="${DIR%/}/bin:$PATH"
fi
