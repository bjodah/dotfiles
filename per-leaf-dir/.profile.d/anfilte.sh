if [ -d "$HOME/vc/anfilte/scripts" ]; then
    export PATH="$HOME/vc/anfilte/scripts:$PATH"
fi
if [ -d /opt/anfilte-cache ]; then
    export ANFILTE_CACHE_DIR=/opt/anfilte-cache
fi
