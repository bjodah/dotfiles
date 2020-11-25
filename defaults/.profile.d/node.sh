# https://nodejs.org/en/download/
# curl -Ls https://nodejs.org/download/release/v12.19.0/node-v12.19.0-linux-x64.tar.xz | tar xJ -C /opt
DIR=$(ls -d /opt/node-v*/ 2>/dev/null | tail -n 1)
if [[ ! -z "$DIR" ]]; then
    export PATH="${DIR%/}/bin:$PATH"
fi
