# git clone git://github.com/emacs-mirror/emacs && cd emacs && git checkout emacs-27 && ./autogen.sh && ./configure --with-modules --prefix=/opt/emacs-27 && make && make install
DIR=$(ls -d /opt/emacs-*/ 2>/dev/null | tail -n 1)
if [[ ! -z "$DIR" ]]; then
    export PATH="${DIR%/}/bin:$PATH"
fi
