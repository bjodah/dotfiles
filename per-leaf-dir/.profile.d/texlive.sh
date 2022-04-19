# https://www.tug.org/texlive/quickinstall.html
DIR=$(ls -d /opt/texlive/20* 2>/dev/null | tail -n 1)
if [[ ! -z "$DIR" ]]; then
    export PATH="${DIR%/}/bin:$PATH"
    export MANPATH="${DIR%/}/texmf-dist/doc/man:$MANPATH"
    export INFOPATH="${DIR%/}/texmf-dist/doc/info:$INFOPATH"
fi
