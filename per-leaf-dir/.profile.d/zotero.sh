# https://www.zotero.org/download/
DIR=$(ls -d /opt/Zotero*/ 2>/dev/null | tail -n 1)
if [ -d "$DIR" ]; then
    export PATH="${DIR%/}:$PATH"
fi
