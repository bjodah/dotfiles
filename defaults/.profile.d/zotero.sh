# https://www.zotero.org/download/
DIR=$(ls -d /opt/Zotero*/ | tail -n 1)
if [ -d "$DIR" ]; then
    export PATH="${DIR%/}/bin:$PATH"
fi
