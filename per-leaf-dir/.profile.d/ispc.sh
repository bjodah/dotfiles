# https://github.com/ispc/ispc/releases/
# curl -Ls https://github.com/ispc/ispc/releases/download/v1.15.0/ispc-v1.15.0-linux.tar.gz | tar xz -C /opt
DIR=$(ls -d /opt/ispc-v*-linux/ 2>/dev/null | tail -n 1)
if [[ ! -z "$DIR" ]]; then
    export PATH="${DIR%/}/bin:$PATH"
fi
