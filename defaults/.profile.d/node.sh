# https://nodejs.org/en/download/
DIR=$(ls -d /opt/node-v*/ | tail -n 1)
if [ -d "$DIR" ]; then
    export PATH="${DIR%/}/bin:$PATH"
fi
