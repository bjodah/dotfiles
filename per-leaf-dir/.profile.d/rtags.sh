# https://github.com/Andersbakken/rtags
DIR=$(ls -d ${HOME}/rovc/rtags/build/bin 2>/dev/null | tail -n 1)
if [[ -d "$DIR" ]]; then
    export PATH="${DIR%/}:$PATH"
fi
